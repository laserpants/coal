{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module: Coal.Compiler.TypeInference

This module implements the type inference system for the Coal language, which is based
on Hindley-Milner type inference with extensions for parametric polymorphism and traits.

The type inference process consists of three main phases:

1. **Kind constraint generation**: Generate kind constraints for type-level constructs
   to ensure well-kindedness of types throughout the module.

2. **Type constraint generation**: Generate type constraints from expressions and
   definitions by traversing the AST and creating equality and explicit constraints
   for type checking.

3. **Constraint solving**: Solve the accumulated type and kind constraints using
   unification, producing a substitution that determines the types of all expressions
   and definitions.

Key features:

* Constraint-based type inference with support for let-polymorphism
* Kind inference for type constructors and type variables
* Trait instance checking and validation
* Type annotation support with parameter inference
* Integration with the compiler's build system for cross-module type information

The module exports the following main functions:

* 'generateKindConstraints': Generate kind constraints for a module
* 'generateConstraints': Generate type constraints for expressions and definitions
* 'define': Add a type binding to the compiler's name store
* 'solveT': Solve all accumulated constraints and produce a type substitution
-}
module Coal.Compiler.TypeInference (
  generateKindConstraints,
  generateConstraints,
  define,
  solveT,
) where

import qualified Coal.Common.Environment as Environment
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (supplied)
import Coal.Compiler.Build
import Coal.Compiler.Build.NameEntry
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.KindEnvironment (moduleKindEnvironment)
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language
import Coal.Language.Module.Path (principalPath)
import Coal.TypeSystem.Constraint (Constraint (Equality, Explicit))
import Coal.TypeSystem.Constraint.Assumption
import Coal.TypeSystem.Constraint.Generation
import Coal.TypeSystem.Constraint.Generation.Stack
import Coal.TypeSystem.Constraint.Solver (solveConstraints)
import Coal.TypeSystem.Kind.Constraint.Generation (EmitKinds (..), runKindConstraintsGen)
import Coal.TypeSystem.Parameterized (Parameterized (..), ToIndexed (..), replaceParamInScheme)
import Coal.TypeSystem.Substitution
import Control.Arrow ((>>>))
import Control.Monad (unless)
import Control.Monad.Except (forM_, throwError)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (get, gets)
import Control.Monad.Writer (MonadWriter, execWriter, tell)
import Data.Data (Data)
import Data.Either.Extra (partitionEithers)
import Data.List.Extra (groupSortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Extras (Dictionary, Name, concatMapM)

generateKindConstraints :: (Monad m) => Module a Kind () -> CompilerT a m ()
generateKindConstraints modul = do
  env <- moduleKindEnvironment modul
  (_, result) <- runKindConstraintsGen env (emitKindConstraints modul)
  let (errors, constraints) = partitionEithers result
  insertKindConstraintsC constraints
  compilerReportKindConstraintsGenErrorsC errors

class GenerateConstraints a c where
  generateConstraints :: (Monad m) => c -> CompilerT a m ()

instance (Data a, Show a) => GenerateConstraints a (Expression a Kind IndexedType) where
  generateConstraints expr = do
    (asms1, cs1) <- generateExpressionConstraints expr
    (asms2, cs2) <- partitionEithers <$> traverse assumptionConstraints asms1
    sub <- gets compilerSubstitution
    insertAssumptionsC (apply sub asms2)
    insertConstraintsC (cs1 <> cs2)

instance (Show a, Data a) => GenerateConstraints a (Definition a Kind IndexedType) where
  generateConstraints =
    \case
      DFunction
        _
        name
        FunctionDefinition
          { functionDefinitionMetadata = loc
          , functionDefinitionType = With _ functionType
          , ..
          } -> do
          insertConstraintsC
            [ Equality
                (RuleTopLevelFunction loc)
                [ functionType
                , typeOf functionDefinitionExpression
                ]
            ]
          expressionType <- freshTypeVariable
          generateConstraints $
            ELet
              loc
              (BFunction loc placeholder functionDefinitionPatterns functionExpr :| mempty)
              (EVariable loc (Label expressionType placeholder))
         where
          placeholder = "#_function__" <> name
          functionExpr =
            case functionDefinitionAnnotation of
              Nothing ->
                functionDefinitionExpression
              Just annotationType ->
                EAnnotation loc annotationType functionDefinitionExpression
      DLet
        _
        name
        LetDefinition
          { letDefinitionMetadata = loc
          , letDefinitionType = With _ letType
          , ..
          } -> do
          insertConstraintsC
            [ Equality
                (RuleTopLevelConstant loc)
                [ letType
                , typeOf letDefinitionExpression
                ]
            ]
          generateConstraints $
            ELet
              loc
              (BPattern loc (PVariable loc (Label letType placeholder)) letExpr :| mempty)
              (EVariable loc (Label letType placeholder))
         where
          placeholder = "#_constant__" <> name
          letExpr =
            case letDefinitionAnnotation of
              Nothing ->
                letDefinitionExpression
              Just annotationType ->
                EAnnotation loc annotationType letDefinitionExpression
      DInstance _ InstanceDefinition{..} -> do
        Build{..} <- getCurrentBuildC
        path <- gets compilerCurrentPath
        case Environment.lookup instanceDefinitionTraitName buildTraits of
          Nothing -> do
            tellErrors [TraitNotInScope instanceDefinitionTraitName (ErrorLocation (principalPath path) instanceDefinitionMetadata)]
            throwError TraitError
          Just TraitEntry{..} ->
            forM_ instanceDefinitionImplementations $
              \case
                d@(DFunction loc name def) ->
                  case Environment.lookup name traitEntryInterface of
                    Nothing -> do
                      tellErrors [UnexpectedTraitDefinition name instanceDefinitionTraitName (ErrorLocation (principalPath path) loc)]
                      throwError TraitError
                    Just sig -> do
                      s <- toIndexedScheme (replaceParamInScheme traitEntryParameter instanceDefinitionType sig)
                      insertConstraintsC [Explicit (RuleTraitInstance loc (typeOf d) s) (typeOf d) s]
                      generateConstraints $ DFunction loc (instanceLabel trait name) def
                d@(DLet loc name def) ->
                  case Environment.lookup name traitEntryInterface of
                    Nothing -> do
                      tellErrors [UnexpectedTraitDefinition name instanceDefinitionTraitName (ErrorLocation (principalPath path) loc)]
                      throwError TraitError
                    Just sig -> do
                      s <- toIndexedScheme (replaceParamInScheme traitEntryParameter instanceDefinitionType sig)
                      insertConstraintsC [Explicit (RuleTraitInstance loc (typeOf d) s) (typeOf d) s]
                      generateConstraints $ DLet loc (instanceLabel trait name) def
                _ ->
                  return ()
           where
            trait = Trait instanceDefinitionTraitName instanceDefinitionType
      _ ->
        return ()

toIndexedScheme :: (Monad m) => Scheme Parameter Kind (Type Parameter Kind) -> CompilerT a m (Scheme TypeIndex Kind IndexedType)
toIndexedScheme Forall{..} = do
  env <- instantiateTypeIndexes schemeTypeVariables
  flip runReaderT (Environment.fromList env) $
    Forall
      <$> toIndexed schemeTypeVariables
      <*> toIndexed schemeTraits
      <*> toIndexed schemeTypeBody

freshTypeVariable :: (Monad m) => CompilerT a m (Type TypeIndex Kind)
freshTypeVariable = supplied (TVariable . TypeIndex KType)

generateExpressionConstraints :: (Monad m, Data a, Show a) => Expression a Kind IndexedType -> CompilerT a m ([CompilerAssumption a], [CompilerConstraint a])
generateExpressionConstraints expr = do
  (assumptions, params, result) <- runConstraintsGen (emitConstraints expr)
  let (errors, constraints) = partitionEithers result
  compilerReportConstraintsGenErrorsC errors
  setTypeAnnotationParamsC params
  return (assumptions, constraints)

runConstraintsGen :: (Monad m) => ConstraintsGenStack a TypeIndex Kind IndexedType r -> CompilerT a m (r, Dictionary (a, TypeIndex Kind), [ConstraintsGenOutput a TypeIndex Kind IndexedType])
runConstraintsGen stack = do
  CompilerState{compilerSupply} <- get
  Build{..} <- getCurrentBuildC
  let (result, ConstraintsGenState{..}, output) =
        runConstraintsGenStack
          compilerSupply
          ( emptyConstraintsGenContext
              { constraintsGenContextDataConstructors =
                  Environment.mapEnvironment dataConstructorEntryConstructor buildDataConstructors
              , constraintsGenContextTypeConstructors =
                  Environment.mapEnvironment typeConstructorEntryKind buildTypeConstructors
                    <> Environment.mapEnvironment (kindOf . aliasEntryType) buildAliases
              }
          )
          stack
  updateSupplyC constraintsGenStateSupply
  return (result, constraintsGenStateAnnotationIndexes, output)

define :: (Monad m) => a -> Name -> IndexedType -> CompilerT a m ()
define loc name t = do
  r <- insertNameC name (Forall (typeIndexesIn s) mempty s)
  unless r $ do
    currentPath <- gets compilerCurrentPath
    tellErrors [NameAlreadyDefined name (ErrorLocation (principalPath currentPath) loc)]
    throwError PreflightFailure
 where
  s = normalizeTypeIndexes t

assumptionConstraints :: (Monad m) => CompilerAssumption a -> CompilerT a m (Either (CompilerAssumption a) (CompilerConstraint a))
assumptionConstraints Assumption{..} = do
  names <- gets compilerNameStore
  return $
    case Environment.lookup assumptionName names of
      Nothing ->
        Left Assumption{..}
      Just s ->
        Right (Explicit (RuleTypeConstraint assumptionMetadata assumptionName assumptionType s) assumptionType s)

solveConstraintsT :: (Monad m, Data a, Eq a) => [CompilerConstraint a] -> CompilerT a m Substitution
solveConstraintsT constraints = do
  dict <- gets compilerTypeAnnotationParams
  n <- gets compilerSupply
  let (sub, m, rs) = solveConstraints n constraints
  updateSupplyC m
  let errors = execWriter (checkTypeAnnotationParameters (Map.toList dict) sub)
  compilerReportSolverRuleViolationsC (apply sub rs)
  compilerReportConstraintsGenErrorsC (EIllFormedTypeAnnotation <$> errors)
  return sub

solveT :: (Monad m, Data a, Eq a) => CompilerT a m Substitution
solveT = do
  constraints <- gets compilerConstraints
  sub1 <- gets compilerSubstitution
  sub2 <- solveConstraintsT constraints
  clearConstraintsC
  clearKindConstraintsC
  clearTypeAnnotationParamsC
  setSubstitutionC (sub2 <> sub1)
  gets compilerSubstitution

checkTypeAnnotationParameters :: (MonadWriter [TypeAnnotationError a] m) => [(Name, (a, TypeIndex Kind))] -> Substitution -> m ()
checkTypeAnnotationParameters ps (Substitution sub) = do
  params <- groupSortOn fst <$> concatMapM go ps
  case filter (lengthMoreThan 1) params of
    [] ->
      pure ()
    qs -> do
      tell [EAnnotationNonDistinctParameter loc p | (_, (p, loc)) <- concat qs]
 where
  lengthMoreThan n = length >>> (> n)
  go (name, (loc, TypeIndex _ index)) =
    case Map.lookup index sub of
      Just (TVariable (TypeIndex _ n)) ->
        pure [(n, (name, loc))]
      Just (TRow (RVariable (TypeIndex _ n))) ->
        pure [(n, (name, loc))]
      Just t -> do
        tell [EAnnotationMonomorphicType loc name t]
        pure []
      Nothing ->
        pure [(index, (name, loc))]
