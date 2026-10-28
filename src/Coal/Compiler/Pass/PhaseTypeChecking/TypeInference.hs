{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Coal.Compiler.Pass.PhaseTypeChecking.TypeInference
Description: Type inference and kind inference for Coal modules

This module implements bidirectional type inference using constraint generation
and solving. It performs both kind inference (ensuring type constructors have
correct kinds) and type inference (inferring types for expressions and patterns).

The inference process:
1. Generate kind constraints and solve them
2. Generate type constraints for each definition
3. Solve constraints incrementally
4. Apply substitutions and normalize types
-}
module Coal.Compiler.Pass.PhaseTypeChecking.TypeInference (passTypeInference) where

import qualified Coal.Common.Environment as Environment
import Coal.Common.FreeVars (freeIn, notConstructor)
import Coal.Common.Label (Label, labelName)
import Coal.Compiler.Build (replaceBuildNameEntry)
import Coal.Compiler.Build.NameEntry (NameEntry (..))
import Coal.Compiler.Error
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT, clearSubstitutionC, insertConstraintsC, updateCurrentBuildC, updateSupplyC)
import Coal.Compiler.State
import Coal.Compiler.TypeInference (checkInstanceConformance, define, generateConstraints, generateKindConstraints, solveT)
import Coal.Language (Expression, HasType (..), IndexedType, Kind, Trait (..), indexed, instanceLabel, rowNormalize, typeOf)
import Coal.Language.Definition
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (principalPath)
import Coal.TypeSystem.Constraint (Constraint (Explicit))
import Coal.TypeSystem.Constraint.Assumption (Assumption (..))
import Coal.TypeSystem.Constraint.Generation.InferenceRule (InferenceRule (..))
import Coal.TypeSystem.Kind.Constraint.Solver (solveKindConstraints)
import Coal.TypeSystem.Kind.Substitution (KindSubstitutable (applyKinds, replaceVariables))
import Coal.TypeSystem.Kind.Unification (KindUnifier (kindUnifierMonad))
import Coal.TypeSystem.Substitution (apply, normalizeTypeIndexes)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (execStateT, get, gets, modify, runState)
import Data.List (partition)
import Data.Set (Set)
import qualified Data.Set as Set
import Extras (Name, forM_)

{- | Type inference compiler pass

Infers kinds and types for all definitions in a module, returning a module
with fully annotated type information.
-}
passTypeInference :: (MonadIO m) => Pass Metadata m (Module Metadata Kind ()) (Module Metadata Kind IndexedType)
passTypeInference = Pass{runPass = passImpl}

passImpl :: (MonadIO m) => Module Metadata Kind () -> CompilerT Metadata m (Module Metadata Kind IndexedType)
passImpl = runTypeInference

runTypeInference :: (MonadIO m) => Module Metadata Kind () -> CompilerT Metadata m (Module Metadata Kind IndexedType)
runTypeInference m = do
  indexedM <- inferKinds m
  newM <- inferTypes indexedM
  replacePlaceholders
  -- Nothing past this point consults the substitution (see 'clearSubstitutionC'),
  -- so release it before the next module is inferred rather than letting every
  -- module's substitution accumulate for the lifetime of the compilation.
  clearSubstitutionC
  return newM

replacePlaceholders :: (Monad m) => CompilerT Metadata m ()
replacePlaceholders = do
  store <- gets compilerNameStore
  updateCurrentBuildC $
    \build ->
      flip execStateT build $
        forM_ (Environment.toList store) $
          \(name, s) ->
            modify (replaceBuildNameEntry (NName name s))

{- | Order module-level definitions so that each definition is inferred after the
definitions it references.

'inferTypes' infers definitions one at a time and stores each definition's type
in the name store as soon as it has been solved. A reference to a definition
that has not been solved yet becomes a /deferred/ assumption (see
'assumptionConstraints'), which leaves the referencing definition's type only
partially known; 'define' then generalizes over the still-free indices and
caches the result, so every later reference to that definition instantiates a
variable that nothing ever resolves. Inferring callees first means no reference
is deferred, so each definition is inferred from complete information.

References that form a cycle (mutual recursion) have no valid order. Those
definitions keep their source order and fall back to the deferred-assumption
path; such cycles are rejected later by
'Coal.Compiler.Pass.PhaseTranslation.DetectCallCycles'.
-}
orderDefinitions :: [Definition Metadata Kind IndexedType] -> [Definition Metadata Kind IndexedType]
orderDefinitions defs = go Set.empty defs
 where
  -- Names bound by a module-level definition. Instance members are bound under
  -- their mangled instance label, mirroring 'storeDefinitionType'.
  provides :: Definition Metadata Kind IndexedType -> [Name]
  provides = \case
    DFunction _ name _ ->
      [name]
    DLet _ name _ ->
      [name]
    DInstance _ InstanceDefinition{instanceDefinitionTraitName, instanceDefinitionType, instanceDefinitionImplementations} ->
      [ instanceLabel trait name
      | member <- instanceDefinitionImplementations
      , name <- memberName member
      ]
     where
      trait = Trait instanceDefinitionTraitName instanceDefinitionType
    _ ->
      []

  boundNames :: Set Name
  boundNames = Set.fromList (concatMap provides defs)

  -- Names a definition references that are bound somewhere in this module.
  -- Self-references are excluded, so a recursive definition does not depend on
  -- itself.
  references :: Definition Metadata Kind IndexedType -> Set Name
  references def =
    (freeNames def `Set.intersection` boundNames) `Set.difference` Set.fromList (provides def)

  freeNames :: Definition Metadata Kind IndexedType -> Set Name
  freeNames = \case
    DFunction _ _ FunctionDefinition{functionDefinitionExpression} ->
      expressionNames functionDefinitionExpression
    DLet _ _ LetDefinition{letDefinitionExpression} ->
      expressionNames letDefinitionExpression
    DInstance _ InstanceDefinition{instanceDefinitionImplementations} ->
      foldMap freeNames instanceDefinitionImplementations
    _ ->
      Set.empty

  expressionNames :: Expression Metadata Kind IndexedType -> Set Name
  expressionNames expr =
    let freeVars :: Set (Label IndexedType)
        freeVars = Set.filter notConstructor (freeIn expr)
     in Set.map labelName freeVars

  memberName :: Definition Metadata Kind IndexedType -> [Name]
  memberName = \case
    DFunction _ name _ ->
      [name]
    DLet _ name _ ->
      [name]
    _ ->
      []

  -- Emit, in source order, every definition whose references are already
  -- available, then repeat. Definitions still pending when nothing is ready
  -- form a cycle: emit them in source order.
  go :: Set Name -> [Definition Metadata Kind IndexedType] -> [Definition Metadata Kind IndexedType]
  go _ [] = []
  go available pending =
    case partition (\def -> references def `Set.isSubsetOf` available) pending of
      (ready@(_ : _), rest) ->
        ready <> go (available <> Set.fromList (concatMap provides ready)) rest
      ([], _) ->
        pending

inferTypes :: (MonadIO m) => Module Metadata Kind () -> CompilerT Metadata m (Module Metadata Kind IndexedType)
inferTypes m = do
  Module{modulePath, moduleExportList, moduleDefinitions} <- assignTypeIndices m

  -- Generate and solve constraints for each definition, callees first (see
  -- 'orderDefinitions').
  forM_ (orderDefinitions moduleDefinitions) $
    \def -> do
      generateConstraints def
      sub <- solveT
      storeDefinitionType (apply sub def)

  CompilerState
    { compilerSubstitution
    , compilerNameStore
    , compilerAssumptions
    } <-
    get

  -- Verify that all assumptions are satisfied
  forM_ compilerAssumptions $
    \Assumption{assumptionMetadata, assumptionName, assumptionType} ->
      case Environment.lookup assumptionName compilerNameStore of
        Nothing -> do
          tellErrors [NameNotInScope assumptionName (ErrorLocation (principalPath modulePath) assumptionMetadata)]
          throwError NoSuchIdentifier
        Just s -> do
          insertConstraintsC [Explicit (RuleAssumptionExplicit assumptionMetadata assumptionName t s) t s]
         where
          t = apply compilerSubstitution assumptionType

  -- Final solve and normalization
  sub <- solveT
  modify (overCompilerAssumptions (apply sub))

  -- Reject instances whose implementations do not have the type the trait
  -- interface requires at the instance's declared (rigid) type.
  forM_ moduleDefinitions (checkInstanceConformance sub)

  let newDefinitions = fmap rowNormalize <$> apply sub moduleDefinitions
  return $
    Module
      { moduleDefinitions = normalizeTypeIndexes newDefinitions
      , ..
      }

inferKinds :: (MonadIO m, Monoid a) => Module a Kind () -> CompilerT a m (Module a Kind ())
inferKinds m = do
  generateKindConstraints m
  constraints <- gets compilerKindConstraints
  case kindUnifierMonad (solveKindConstraints constraints) of
    Left err -> do
      -- Kind inference failed - report as KindError
      tellErrors [KindError err (ErrorLocation (principalPath (modulePath m)) mempty)]
      throwError CompilerError
    Right sub -> do
      modify (overCompilerNameStore (replaceVariables . applyKinds sub))
      modify (overCompilerModuleWithPath (modulePath m) (replaceVariables . applyKinds sub))
      return (replaceVariables (applyKinds sub m))

storeDefinitionType :: (Monad m) => Definition Metadata Kind IndexedType -> CompilerT Metadata m ()
storeDefinitionType =
  \case
    def@(DFunction loc name _) ->
      define loc name (typeOf def)
    def@(DLet loc name _) ->
      define loc name (typeOf def)
    DInstance
      _
      InstanceDefinition
        { instanceDefinitionTraitName
        , instanceDefinitionMetadata
        , instanceDefinitionType
        , instanceDefinitionImplementations
        } -> do
        let trait = Trait instanceDefinitionTraitName instanceDefinitionType
        forM_ instanceDefinitionImplementations $ \case
          def@(DFunction _ name _) ->
            define instanceDefinitionMetadata (instanceLabel trait name) (typeOf def)
          def@(DLet _ name _) ->
            define instanceDefinitionMetadata (instanceLabel trait name) (typeOf def)
          _ ->
            return ()
    -- Type definitions, aliases, traits don't need type storage
    _ ->
      return ()

assignTypeIndices :: (Monad m, Traversable t) => t e -> CompilerT Metadata m (t IndexedType)
assignTypeIndices ds = do
  CompilerState{compilerSupply} <- get
  let (result, supply) = runState (indexed ds) compilerSupply
  updateSupplyC supply
  return result
