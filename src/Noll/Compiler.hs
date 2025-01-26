{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler (
  CompilerEnvironment (..),
  insertNamesC,
  Compiler (..),
  CompilerState (..),
  runCompiler,
  evalCompiler,
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
import Control.Monad.State (MonadState, State, gets, modify, runState)
import Control.Monad.Writer (execWriter)
import Data.Either.Extra (partitionEithers)
import Noll.Common.Environment (Environment (..))
import Noll.Common.List1 (NonEmpty ((:|)))
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Constant (..),
  Constructor (..),
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
import Noll.Utils (Dictionary, Name, (<$$>))

import qualified Data.Map.Strict as Map
import qualified Noll.Common.Environment as Environment

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
  , compilerSubstitution :: Substitution
  , compilerSupply :: Int
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE overCompilerStateConstraintsGenErrors #-}
overCompilerStateConstraintsGenErrors :: ([ConstraintsGenError a] -> [ConstraintsGenError a]) -> CompilerState a -> CompilerState a
overCompilerStateConstraintsGenErrors fn CompilerState{..} = CompilerState{compilerConstraintsGenErrors = fn compilerConstraintsGenErrors, ..}

{-# INLINE overCompilerTypeAnnotationParams #-}
overCompilerTypeAnnotationParams :: (Dictionary (a, TypeIndex Kind) -> Dictionary (a, TypeIndex Kind)) -> CompilerState a -> CompilerState a
overCompilerTypeAnnotationParams fn CompilerState{..} = CompilerState{compilerTypeAnnotationParams = fn compilerTypeAnnotationParams, ..}

{-# INLINE overCompilerSolverRuleViolations #-}
overCompilerSolverRuleViolations :: ([InferenceRule Kind a] -> [InferenceRule Kind a]) -> CompilerState a -> CompilerState a
overCompilerSolverRuleViolations fn CompilerState{..} = CompilerState{compilerSolverRuleViolations = fn compilerSolverRuleViolations, ..}

{-# INLINE overCompilerNameEnvironment #-}
overCompilerNameEnvironment :: (Environment (Scheme TypeIndex Kind IndexedType) -> Environment (Scheme TypeIndex Kind IndexedType)) -> CompilerState a -> CompilerState a
overCompilerNameEnvironment fn CompilerState{..} = CompilerState{compilerNameEnvironment = fn compilerNameEnvironment, ..}

{-# INLINE overCompilerSupply #-}
overCompilerSupply :: (Int -> Int) -> CompilerState a -> CompilerState a
overCompilerSupply fn CompilerState{..} = CompilerState{compilerSupply = fn compilerSupply, ..}

{-# INLINE initialCompilerState #-}
initialCompilerState :: CompilerState a
initialCompilerState =
  CompilerState
    { compilerConstraintsGenErrors = []
    , compilerTypeAnnotationParams = mempty
    , compilerSolverRuleViolations = []
    , compilerNameEnvironment = mempty
    , compilerSubstitution = mempty
    , compilerSupply = 0
    }

newtype Compiler a c = Compiler {compilerStack :: ReaderT CompilerEnvironment (State (CompilerState a)) c}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader CompilerEnvironment
    , MonadState (CompilerState a)
    )

{-# INLINE compilerReportConstraintsGenErrors #-}
compilerReportConstraintsGenErrors :: [ConstraintsGenError a] -> Compiler a ()
compilerReportConstraintsGenErrors errors = modify (overCompilerStateConstraintsGenErrors (<> errors))

{-# INLINE compilerSetTypeAnnotationParams #-}
compilerSetTypeAnnotationParams :: Dictionary (a, TypeIndex Kind) -> Compiler a ()
compilerSetTypeAnnotationParams params = modify (overCompilerTypeAnnotationParams (const params))

{-# INLINE compilerReportSolverRuleViolations #-}
compilerReportSolverRuleViolations :: [InferenceRule Kind a] -> Compiler a ()
compilerReportSolverRuleViolations errors = modify (overCompilerSolverRuleViolations (<> errors))

{-# INLINE getConstraintsGenErrorsC #-}
getConstraintsGenErrorsC :: Compiler a [ConstraintsGenError a]
getConstraintsGenErrorsC = gets compilerConstraintsGenErrors

{-# INLINE getSolverRuleViolationsC #-}
getSolverRuleViolationsC :: Compiler a [InferenceRule Kind a]
getSolverRuleViolationsC = gets compilerSolverRuleViolations

{-# INLINE insertNameC #-}
insertNameC :: Name -> Scheme TypeIndex Kind IndexedType -> Compiler a ()
insertNameC name scheme = modify (overCompilerNameEnvironment (Environment.insert name scheme))

{-# INLINE insertNamesC #-}
insertNamesC :: [(Name, Scheme TypeIndex Kind IndexedType)] -> Compiler a ()
insertNamesC names = modify (overCompilerNameEnvironment (Environment.insertMany names))

{-# INLINE updateSupplyC #-}
updateSupplyC :: Int -> Compiler a ()
updateSupplyC supply = modify (overCompilerSupply (const supply))

{-# INLINE runCompiler #-}
runCompiler :: CompilerEnvironment -> Compiler a c -> (c, CompilerState a)
runCompiler env com = runState (runReaderT (compilerStack com) env) initialCompilerState

{-# INLINE evalCompiler #-}
evalCompiler :: CompilerEnvironment -> Compiler a c -> c
evalCompiler = fst <$$> runCompiler

type ConstraintsGenResult c o k t r = (r, Dictionary (c, o k), [ConstraintsGenOutput c o k t])

runConstraintsGenC :: Int -> ConstraintsGenStack c TypeIndex Kind IndexedType r -> Compiler a (ConstraintsGenResult c TypeIndex Kind IndexedType r)
runConstraintsGenC index stack = do
  env <- ask
  let (result, ConstraintsGenState{..}, output) = runConstraintsGenStack index (context env) stack
  updateSupplyC constraintsGenerationStateSupply
  pure (result, constraintsGenerationStateTypeIndexes, output)
 where
  context CompilerEnvironment{..} =
    ConstraintsGenContext
      { constraintsGenerationContextMonomorphicSet = mempty
      , constraintsGenerationContextDataConstructorEnv = compilerDataConstructorEnv
      , constraintsGenerationContextTypeConstructorEnv = compilerTypeConstructorEnv
      }

type CompilerAssumption = Assumption IndexedType

type CompilerConstraint a = Constraint (InferenceRule Kind a) TypeIndex Kind IndexedType

generateConstraintsC :: Expression a IndexedType -> Compiler a ([CompilerAssumption], [CompilerConstraint a])
generateConstraintsC e = do
  (assumptions, params, result) <- runConstraintsGenC (freshIdIn e) (collectConstraints e)
  let (errors, constraints) = partitionEithers result
  compilerReportConstraintsGenErrors errors
  compilerSetTypeAnnotationParams params
  pure (assumptions, constraints)

assumptionConstraints :: CompilerAssumption -> Compiler a (Either CompilerAssumption (CompilerConstraint a))
assumptionConstraints Assumption{..} = do
  names <- gets compilerNameEnvironment
  case Environment.lookup assumptionName names of
    Nothing ->
      pure $ Left Assumption{..}
    Just s ->
      pure $ Right (Explicit (InferenceRule 200) assumptionType s)

solveConstraintsC :: (Show a, Eq a) => [CompilerConstraint a] -> Compiler a Substitution
solveConstraintsC cs = do
  dict <- gets compilerTypeAnnotationParams
  let (sub, rs) = solveConstraints cs
      errors = execWriter (checkTypeAnnotationParameters (Map.toList dict) sub)
  compilerReportSolverRuleViolations (apply sub rs)
  compilerReportConstraintsGenErrors (IllFormedTypeAnnotation <$> errors)
  pure sub

compileConstraintsC ::
  ( Functor f
  , Substitutable (f IndexedType)
  , TypeIndexed Kind (f IndexedType)
  , Show a
  , Eq a
  ) =>
  [CompilerConstraint a] ->
  f IndexedType ->
  Expression a IndexedType ->
  Compiler a (f IndexedType, [CompilerAssumption])
compileConstraintsC cs o e = do
  (as0, cs0) <- generateConstraintsC e
  (as1, cs1) <- partitionEithers <$> traverse assumptionConstraints as0
  sub <- solveConstraintsC (cs <> cs0 <> cs1)
  pure (normalizeTypeIndexes (normalizeRowTypes <$> apply sub o), apply sub as1)

typeCheckExpressionC ::
  (Show a, Eq a) =>
  Expression a IndexedType ->
  Compiler a (Expression a IndexedType, [CompilerAssumption])
typeCheckExpressionC e = compileConstraintsC [] e e

typeCheckFunctionC ::
  (Show a, Eq a) =>
  Function Expression a IndexedType ->
  Compiler a (Function Expression a IndexedType, [CompilerAssumption])
typeCheckFunctionC f@(Function loc (Uses _ t) ps e) = do
  compileConstraintsC [Equality (InferenceRule 999) [t, typeOf e]] f $
    ELet
      loc
      (BFunction loc placeholder ps e :| [])
      (EVariable loc (Label (foldType t (typeOf <$> ps)) placeholder))
 where
  placeholder = "$$$.function"

typeCheckConstantC ::
  (Show a, Eq a) =>
  Constant Expression a IndexedType ->
  Compiler a (Constant Expression a IndexedType, [CompilerAssumption])
typeCheckConstantC g@(Constant loc (Uses _ t) e) = do
  compileConstraintsC [Equality (InferenceRule 999) [t, typeOf e]] g $
    ELet
      loc
      (BPattern loc (PVariable loc (Label t placeholder)) e :| [])
      (EVariable loc (Label t placeholder))
 where
  placeholder = "$$$.constant"

typeCheckModuleC = do
  undefined

