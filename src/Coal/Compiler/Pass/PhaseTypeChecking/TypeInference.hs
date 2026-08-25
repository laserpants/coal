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
import Coal.Compiler.Build (replaceBuildNameEntry)
import Coal.Compiler.Build.NameEntry (NameEntry (..))
import Coal.Compiler.Error
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT, insertConstraintsC, updateCurrentBuildC, updateSupplyC)
import Coal.Compiler.State
import Coal.Compiler.TypeInference (define, generateConstraints, generateKindConstraints, solveT)
import Coal.Language (HasType (..), IndexedType, Kind, Trait (..), indexed, instanceLabel, rowNormalize, typeOf)
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
import Extras (forM_)

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

inferTypes :: (MonadIO m) => Module Metadata Kind () -> CompilerT Metadata m (Module Metadata Kind IndexedType)
inferTypes m = do
  Module{modulePath, moduleExportList, moduleDefinitions} <- assignTypeIndices m

  -- Generate and solve constraints for each definition
  forM_ moduleDefinitions $
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
