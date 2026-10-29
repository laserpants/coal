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
  checkInstanceConformance,
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
import Coal.TypeSystem.Kind.Constraint (KindConstraintError (..))
import Coal.TypeSystem.Kind.Constraint.Generation (EmitKinds (..), runKindConstraintsGen)
import Coal.TypeSystem.Parameterized (Parameterized (..), ToIndexed (..), replaceParamInScheme)
import Coal.TypeSystem.Substitution
import Coal.TypeSystem.Unification (runUnifier, unify)
import Control.Arrow ((>>>))
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (get, gets)
import Control.Monad.Writer (MonadWriter, execWriter, tell)
import Data.Data (Data)
import Data.Either.Extra (partitionEithers)
import Data.List.Extra (groupSortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Extras (Dictionary, Name, concatMapM, forM_)

generateKindConstraints :: (Monad m, Monoid a) => Module a Kind () -> CompilerT a m ()
generateKindConstraints modul@Module{moduleDefinitions} = do
  env <- moduleKindEnvironment modul
  -- Kind checking must see every type constructor that is actually in scope.
  -- 'moduleKindEnvironment' derives its entries from the module's definitions
  -- and imports, but an imported type alias is expanded (see
  -- 'Coal.Compiler.Pass.PhaseTypeChecking.ExpandAliases') to a type that may
  -- mention type constructors which travel with the alias rather than being
  -- imported by name (mirroring the recursive insertion performed by
  -- 'Coal.Compiler.Pass.PhaseTypeChecking.PrepareBuild.insertTypeName'). Those
  -- constructors are recorded in the build, so fold the build's type
  -- constructors, traits and aliases into the environment. The module-derived
  -- entries win on conflict.
  Build{..} <- getCurrentBuildC
  let traitKindOf e = KArrow (kindOf (traitEntryParameter e)) KTrait
      buildEnv =
        Environment.mapEnvironment typeConstructorEntryKind buildTypeConstructors
          <> Environment.mapEnvironment traitKindOf buildTraits
          <> Environment.mapEnvironment (kindOf . aliasEntryType) buildAliases
      genEnv = env <> buildEnv
  -- Generate one definition at a time, so that each constraint and error can be
  -- tagged with its definition's location (see 'KindConstraint'): a failing
  -- constraint is then reported against the definition that produced it, which
  -- the kind-indexed type AST itself cannot supply. The constraints are still
  -- solved together, in the order they are emitted here.
  forM_ moduleDefinitions $ \def -> do
    (_, result) <- runKindConstraintsGen genEnv (emitKindConstraints def)
    let (errors, constraints) = partitionEithers result
        loc = definitionMetadata def
    insertKindConstraintsC (fmap (const loc) <$> constraints)
    compilerReportKindConstraintsGenErrorsC (KindConstraintError loc <$> errors)

{- | The source location of a definition.

Every definition constructor stores its metadata first, so the function is total;
imports carry a location too, even though they emit no kind constraints.
-}
definitionMetadata :: Definition a k () -> a
definitionMetadata =
  \case
    DType a _ _ ->
      a
    DTypeAlias a _ _ ->
      a
    DFunction a _ _ ->
      a
    DFunctionGroup a _ _ ->
      a
    DFold a _ _ ->
      a
    DLet a _ _ ->
      a
    DImport a _ _ ->
      a
    DNamespaceImport a _ ->
      a
    DTrait a _ _ ->
      a
    DInstance a _ ->
      a

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
toIndexedScheme = fmap fst . toIndexedSchemeWithEnv

{- | Like 'toIndexedScheme', but also returns the type index that was
assigned to each of the scheme's type variables.
-}
toIndexedSchemeWithEnv :: (Monad m) => Scheme Parameter Kind (Type Parameter Kind) -> CompilerT a m (Scheme TypeIndex Kind IndexedType, [(Name, TypeIndex Kind)])
toIndexedSchemeWithEnv Forall{..} = do
  env <- instantiateTypeIndexes schemeTypeVariables
  s <-
    flip runReaderT (Environment.fromList env) $
      Forall
        <$> toIndexed schemeTypeVariables
        <*> toIndexed schemeTraits
        <*> toIndexed schemeTypeBody
  pure (s, env)

{- | Verify that each trait-instance implementation has the type that the
trait interface requires at the instance's declared type.

Every type variable that the substituted interface signature quantifies is
rigid: the instance head's parameters, and the member's own type variables. The
implementation may not instantiate any of them with a compound type, because
that would make it /less/ polymorphic than the interface promises.

The conformance constraint ('RuleTraitInstance') alone does not enforce this: it
is solved by instantiating the signature with flexible unification variables, so
it only requires the implementation to unify with /some/ instance of the
signature — an implementation that is more specific is accepted. Two shapes of
that error are rejected here.

An instance head that is a bare variable, as in

> instance Functor<f> with (FreeMonad<f>) { let map = map_free }

is accepted by the flexible instantiation, which unifies @f@ with @Free<f, ·>@.
The member then needs a @FreeMonad<f>@ dictionary that its recorded type does not
mention, so no dictionary is inserted at use sites and the generated code is
inconsistent: the member is stored with the interface's arity while its compiled
definition takes the dictionary as an extra parameter.

A closed instance head, as in

> type LangF<a> = ReadLn(string -> a)
> instance Functor<LangF> { fun map(f, v) = ... g(f(x)) ... }

is accepted for the same reason, but the head contributes no variables, so only
the member's own @a@ and @b@ can be instantiated: the body forces them to
@string@, and the implementation is compiled at
@(string -> string) -> LangF<string> -> LangF<string>@ while use sites type the
member from the interface (@a -> b@ at any @a@ and @b@). The two disagree, and
the mismatch reaches the generated code.

The check is a local unification whose substitution is discarded, so it cannot
affect the inferred types. A mismatch is reported through the solver-rule
machinery, which 'passReportTypeErrors' turns into a type error.
-}
checkInstanceConformance :: (Monad m, Data a) => Substitution -> Definition a Kind IndexedType -> CompilerT a m ()
checkInstanceConformance sub =
  \case
    DInstance _ InstanceDefinition{..} -> do
      Build{buildTraits} <- getCurrentBuildC
      case Environment.lookup instanceDefinitionTraitName buildTraits of
        Nothing ->
          pure ()
        Just TraitEntry{traitEntryParameter, traitEntryInterface} -> do
          forM_ instanceDefinitionImplementations $ \def ->
            forM_ (memberName def) $ \(loc, name) ->
              forM_ (Environment.lookup name traitEntryInterface) $ \sig -> do
                (expected, env) <- toIndexedSchemeWithEnv (replaceParamInScheme traitEntryParameter instanceDefinitionType sig)
                let actual = apply sub (typeOf def)
                    rigid = snd <$> env
                seed <- gets compilerSupply
                let (result, seed_) = runUnifier seed (unify (schemeTypeBody expected) actual)
                updateSupplyC seed_
                case result of
                  Left{} ->
                    -- Already reported by the 'RuleTraitInstance' constraint.
                    pure ()
                  Right localSub ->
                    forM_ rigid $ \index ->
                      unless (isTypeVariable (apply localSub (TVariable index))) $
                        compilerReportSolverRuleViolationsC [RuleTraitInstance loc actual expected]
    _ ->
      pure ()
 where
  memberName =
    \case
      DFunction loc name _ -> Just (loc, name)
      DLet loc name _ -> Just (loc, name)
      _ -> Nothing

  isTypeVariable =
    \case
      TVariable{} -> True
      _ -> False

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
