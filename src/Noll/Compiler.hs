{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler (
  CompilerEnvironment (..),
  insertNamesC,
  CompilerT (..),
  CompilerState (..),
  runCompilerT,
  evalCompilerT,
  runConstraintsGenC,
  generateConstraintsC,
  typeCheckExpressionC,
  typeCheckFunctionC,
  typeCheckConstantC,
  solveConstraintsC,
  getConstraintsGenErrorsC,
  getSolverRuleViolationsC,
) where

import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState, StateT, gets, modify, runStateT)
import Control.Monad.Writer (execWriter)
import Data.Either.Extra (partitionEithers)
import Data.Foldable (traverse_)
import Noll.Common.Environment (Environment (..))
import Noll.Common.List1 (NonEmpty ((:|)))
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Constant (..),
  Constructor (..),
  Definition (..),
  Expression (..),
  Function (..),
  IndexedType,
  Kind (..),
  Pattern (..),
  Scheme (..),
  TypeIndex (..),
  TypeIndexed (..),
  Uses (..),
  foldType,
  freshIdIn,
  normalizeRowTypes,
  typeOf,
 )
import Noll.SystemF (
  Assumption (..),
  Constraint (..),
  ConstraintsGenContext (..),
  ConstraintsGenError (..),
  ConstraintsGenOutput,
  ConstraintsGenStack (..),
  ConstraintsGenState (..),
  InferenceRule (..),
  Substitutable (..),
  Substitution (..),
  checkTypeAnnotationParameters,
  collectConstraints,
  normalizeTypeIndexes,
  runConstraintsGenStack,
  solveConstraints,
 )
import Noll.Utils (Dictionary, Name, Over, (<$$$>))

import qualified Data.Map.Strict as Map
import qualified Noll.Common.Environment as Environment

type CompilerAssumption = Assumption IndexedType

type CompilerConstraint a = Constraint (InferenceRule Kind a) TypeIndex Kind IndexedType

data CompilerEnvironment = CompilerEnvironment
  { compilerDataConstructorEnv :: Environment (Constructor TypeIndex Kind IndexedType)
  , compilerTypeConstructorEnv :: Environment Kind
  }
  deriving (Show, Eq, Ord, Read)

data CompilerState a = CompilerState
  { compilerConstraintsGenErrors :: [ConstraintsGenError a]
  , compilerTypeAnnotationParams :: Dictionary (a, TypeIndex Kind)
  , compilerSolverRuleViolations :: [InferenceRule Kind a]
  , compilerNameEnvironment :: Environment (Scheme TypeIndex Kind IndexedType)
  , compilerAssumptions :: [CompilerAssumption]
  , compilerSubstitution :: Substitution
  , compilerSupply :: Int
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE overCompilerStateConstraintsGenErrors #-}
overCompilerStateConstraintsGenErrors :: Over (CompilerState a) [ConstraintsGenError a]
overCompilerStateConstraintsGenErrors fn CompilerState{..} = CompilerState{compilerConstraintsGenErrors = fn compilerConstraintsGenErrors, ..}

{-# INLINE overCompilerTypeAnnotationParams #-}
overCompilerTypeAnnotationParams :: Over (CompilerState a) (Dictionary (a, TypeIndex Kind))
overCompilerTypeAnnotationParams fn CompilerState{..} = CompilerState{compilerTypeAnnotationParams = fn compilerTypeAnnotationParams, ..}

{-# INLINE overCompilerSolverRuleViolations #-}
overCompilerSolverRuleViolations :: Over (CompilerState a) [InferenceRule Kind a]
overCompilerSolverRuleViolations fn CompilerState{..} = CompilerState{compilerSolverRuleViolations = fn compilerSolverRuleViolations, ..}

{-# INLINE overCompilerNameEnvironment #-}
overCompilerNameEnvironment :: Over (CompilerState a) (Environment (Scheme TypeIndex Kind IndexedType))
overCompilerNameEnvironment fn CompilerState{..} = CompilerState{compilerNameEnvironment = fn compilerNameEnvironment, ..}

{-# INLINE overCompilerSupply #-}
overCompilerSupply :: Over (CompilerState a) Int
overCompilerSupply fn CompilerState{..} = CompilerState{compilerSupply = fn compilerSupply, ..}

{-# INLINE overCompilerAssumptions #-}
overCompilerAssumptions :: Over (CompilerState a) [CompilerAssumption]
overCompilerAssumptions fn CompilerState{..} = CompilerState{compilerAssumptions = fn compilerAssumptions, ..}

{-# INLINE overCompilerSubstitution #-}
overCompilerSubstitution :: Over (CompilerState a) Substitution
overCompilerSubstitution fn CompilerState{..} = CompilerState{compilerSubstitution = fn compilerSubstitution, ..}

{-# INLINE initialCompilerState #-}
initialCompilerState :: CompilerState a
initialCompilerState =
  CompilerState
    { compilerConstraintsGenErrors = []
    , compilerTypeAnnotationParams = mempty
    , compilerSolverRuleViolations = []
    , compilerNameEnvironment = mempty
    , compilerAssumptions = []
    , compilerSubstitution = mempty
    , compilerSupply = 0
    }

newtype CompilerT a m c = Compiler {compilerStack :: ReaderT CompilerEnvironment (StateT (CompilerState a) m) c}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader CompilerEnvironment
    , MonadState (CompilerState a)
    )

{-# INLINE compilerReportConstraintsGenErrors #-}
compilerReportConstraintsGenErrors :: (Monad m) => [ConstraintsGenError a] -> CompilerT a m ()
compilerReportConstraintsGenErrors errors = modify (overCompilerStateConstraintsGenErrors (<> errors))

{-# INLINE compilerSetTypeAnnotationParams #-}
compilerSetTypeAnnotationParams :: (Monad m) => Dictionary (a, TypeIndex Kind) -> CompilerT a m ()
compilerSetTypeAnnotationParams params = modify (overCompilerTypeAnnotationParams (const params))

{-# INLINE compilerReportSolverRuleViolations #-}
compilerReportSolverRuleViolations :: (Monad m) => [InferenceRule Kind a] -> CompilerT a m ()
compilerReportSolverRuleViolations errors = modify (overCompilerSolverRuleViolations (<> errors))

{-# INLINE getConstraintsGenErrorsC #-}
getConstraintsGenErrorsC :: (Monad m) => CompilerT a m [ConstraintsGenError a]
getConstraintsGenErrorsC = gets compilerConstraintsGenErrors

{-# INLINE getSolverRuleViolationsC #-}
getSolverRuleViolationsC :: (Monad m) => CompilerT a m [InferenceRule Kind a]
getSolverRuleViolationsC = gets compilerSolverRuleViolations

{-# INLINE insertNameC #-}
insertNameC :: (Monad m) => Name -> Scheme TypeIndex Kind IndexedType -> CompilerT a m ()
insertNameC name scheme = modify (overCompilerNameEnvironment (Environment.insert name scheme))

{-# INLINE insertNamesC #-}
insertNamesC :: (Monad m) => [(Name, Scheme TypeIndex Kind IndexedType)] -> CompilerT a m ()
insertNamesC names = modify (overCompilerNameEnvironment (Environment.insertMultiple names))

{-# INLINE updateSupplyC #-}
updateSupplyC :: (Monad m) => Int -> CompilerT a m ()
updateSupplyC supply = modify (overCompilerSupply (const supply))

{-# INLINE updateSubstitutionC #-}
updateSubstitutionC :: (Monad m) => Substitution -> CompilerT a m ()
updateSubstitutionC sub = modify (overCompilerSubstitution (const sub))

{-# INLINE insertAssumptionsC #-}
insertAssumptionsC :: (Monad m) => [CompilerAssumption] -> CompilerT a m ()
insertAssumptionsC as = modify (overCompilerAssumptions (<> as))

{-# INLINE runCompilerT #-}
runCompilerT :: (Monad m) => CompilerEnvironment -> CompilerT a m c -> m (c, CompilerState a)
runCompilerT env com = runStateT (runReaderT (compilerStack com) env) initialCompilerState

{-# INLINE evalCompilerT #-}
evalCompilerT :: (Monad m) => CompilerEnvironment -> CompilerT a m c -> m c
evalCompilerT = fst <$$$> runCompilerT

type ConstraintsGenResult c o k t r = (r, Dictionary (c, o k), [ConstraintsGenOutput c o k t])

runConstraintsGenC :: (Monad m) => Int -> ConstraintsGenStack c TypeIndex Kind IndexedType r -> CompilerT a m (ConstraintsGenResult c TypeIndex Kind IndexedType r)
runConstraintsGenC index stack = do
  env <- ask
  let (result, ConstraintsGenState{..}, output) = runConstraintsGenStack index (context env) stack
  updateSupplyC constraintsGenStateSupply
  pure (result, constraintsGenStateTypeIndexes, output)
 where
  context CompilerEnvironment{..} =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextDataConstructorEnv = compilerDataConstructorEnv
      , constraintsGenContextTypeConstructorEnv = compilerTypeConstructorEnv
      }

generateConstraintsC :: (Monad m) => Expression a IndexedType -> CompilerT a m ([CompilerAssumption], [CompilerConstraint a])
generateConstraintsC e = do
  (assumptions, params, result) <- runConstraintsGenC (freshIdIn e) (collectConstraints e)
  let (errors, constraints) = partitionEithers result
  compilerReportConstraintsGenErrors errors
  compilerSetTypeAnnotationParams params
  pure (assumptions, constraints)

assumptionConstraints :: (Monad m) => CompilerAssumption -> CompilerT a m (Either CompilerAssumption (CompilerConstraint a))
assumptionConstraints Assumption{..} = do
  names <- gets compilerNameEnvironment
  case Environment.lookup assumptionName names of
    Nothing ->
      pure $ Left Assumption{..}
    Just s ->
      pure $ Right (Explicit (InferenceRule 200) assumptionType s)

solveConstraintsC :: (Monad m, Show a, Eq a) => [CompilerConstraint a] -> CompilerT a m Substitution
solveConstraintsC cs = do
  dict <- gets compilerTypeAnnotationParams
  let (sub, rs) = solveConstraints cs
      errors = execWriter (checkTypeAnnotationParameters (Map.toList dict) sub)
  compilerReportSolverRuleViolations (apply sub rs)
  compilerReportConstraintsGenErrors (IllFormedTypeAnnotation <$> errors)
  pure sub

solveC :: (Monad m, Show a, Eq a) => [CompilerConstraint a] -> CompilerT a m ()
solveC constraints = do
  sub1 <- gets compilerSubstitution
  sub2 <- solveConstraintsC constraints
  updateSubstitutionC (sub2 <> sub1)

compileConstraintsC ::
  ( Functor f
  , Monad m
  , Substitutable (f IndexedType)
  , TypeIndexed Kind (f IndexedType)
  , Show a
  , Eq a
  ) =>
  [CompilerConstraint a] ->
  f IndexedType ->
  Expression a IndexedType ->
  CompilerT a m ()
compileConstraintsC cs o e = do
  (ms0, cs0) <- generateConstraintsC e
  (ms1, cs1) <- partitionEithers <$> traverse assumptionConstraints ms0
  sub <- gets compilerSubstitution
  insertAssumptionsC (apply sub ms1)
  solveC (cs <> cs0 <> cs1)

typeCheckExpressionC ::
  (Monad m, Show a, Eq a) =>
  Expression a IndexedType ->
  CompilerT a m (Expression a IndexedType, [CompilerAssumption])
typeCheckExpressionC e = do
  compileConstraintsC [] e e
  ams <- gets compilerAssumptions
  sub <- gets compilerSubstitution
  pure (normalizeTypeIndexes (normalizeRowTypes <$> apply sub e), apply sub ams)

compileFunctionC ::
  (Monad m, Show a, Eq a) =>
  Function Expression a IndexedType ->
  CompilerT a m ()
compileFunctionC f@(Function loc (Uses _ t) ps e) =
  compileConstraintsC [Equality (InferenceRule 999) [t, typeOf e]] f $
    ELet
      loc
      (BFunction loc placeholder ps e :| [])
      (EVariable loc (Label (foldType t (typeOf <$> ps)) placeholder))
 where
  placeholder = "###.function"

typeCheckFunctionC ::
  (Monad m, Show a, Eq a) =>
  Function Expression a IndexedType ->
  CompilerT a m (Function Expression a IndexedType, [CompilerAssumption])
typeCheckFunctionC f = do
  compileFunctionC f
  ams <- gets compilerAssumptions
  sub <- gets compilerSubstitution
  pure (normalizeTypeIndexes (normalizeRowTypes <$> apply sub f), ams)

compileConstantC ::
  (Monad m, Show a, Eq a) =>
  Constant Expression a IndexedType ->
  CompilerT a m ()
compileConstantC g@(Constant loc (Uses _ t) e) =
  compileConstraintsC [Equality (InferenceRule 999) [t, typeOf e]] g $
    ELet
      loc
      (BPattern loc (PVariable loc (Label t placeholder)) e :| [])
      (EVariable loc (Label t placeholder))
 where
  placeholder = "###.constant"

typeCheckConstantC ::
  (Monad m, Show a, Eq a) =>
  Constant Expression a IndexedType ->
  CompilerT a m (Constant Expression a IndexedType, [CompilerAssumption])
typeCheckConstantC c = do
  compileConstantC c
  ams <- gets compilerAssumptions
  sub <- gets compilerSubstitution
  pure (normalizeTypeIndexes (normalizeRowTypes <$> apply sub c), ams)

typeCheckModuleC = do
  undefined

typeCheckiDefinitionsC :: (Monad m, Show a, Eq a) => [Definition a k IndexedType] -> CompilerT a m ()
typeCheckiDefinitionsC = traverse_ typeCheckDefinitionC

typeCheckDefinitionC :: (Monad m, Show a, Eq a) => Definition a k IndexedType -> CompilerT a m ()
typeCheckDefinitionC =
  \case
    DFunction _ f ->
      compileFunctionC f
