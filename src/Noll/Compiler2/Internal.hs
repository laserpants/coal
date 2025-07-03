{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler2.Internal (
  Compiler2T (..),
  Compiler2Environment (..),
  Compiler2State (..),
  CompilerConstraint,
  CompilerAssumption,
  runCompiler2T,
  evalCompiler2T,
  insertNameC,
  insertNamesC,
  insertConstraintsC,
  insertAssumptionsC,
  updateSubstitutionC,
  clearConstraintsC,
  updateSupply,
  updateSupplyC,
  insertSupplyC,
  compiler2ReportConstraintsGenErrors,
  compiler2ReportSolverRuleViolations,
  compiler2SetTypeAnnotationParams,
)
where

import Control.Monad.RWS (RWST, runRWST)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState, modify)
import Lang.Common.Environment (Environment (..))
import Lang.Common.Supply (Supply (..))
import Lang.Utils (Dictionary, Name, Over, (<$$$>))
import Noll.Compiler.Transform.Type.AliasExpansion
import Noll.Language
import Noll.SystemF

import qualified Lang.Common.Environment as Environment

type TraitImplementationEnv = Environment (Scheme TypeIndex Kind IndexedType)

data Compiler2Environment o k t = Compiler2Environment
  { compiler2DataConstructorEnv :: Environment (Constructor o k t)
  , compiler2TypeConstructorEnv :: Environment Kind
  , compiler2TraitEnvironment :: Environment (TypeIndex Kind, TraitImplementationEnv)
  , compiler2TraitEnv :: Environment (o k, Environment (Scheme o k t))
  , compiler2AliasEnv :: AliasEnvironment
  }
  deriving (Show, Eq, Ord, Read)

type CompilerConstraint a = Constraint (InferenceRule Kind a) TypeIndex Kind IndexedType

type CompilerAssumption = Assumption IndexedType

data Compiler2State a = Compiler2State
  { compiler2Supply :: Int
  , compiler2NameStore :: Environment (Scheme TypeIndex Kind IndexedType)
  , compiler2Substitution :: Substitution
  , compiler2Constraints :: [CompilerConstraint a]
  , compiler2ConstraintsGenErrors :: [ConstraintsGenError a]
  , compiler2SolverRuleViolations :: [InferenceRule Kind a]
  , compiler2Assumptions :: [CompilerAssumption]
  , compiler2TypeAnnotationParams :: Dictionary (a, TypeIndex Kind)
  }
  deriving (Show, Eq, Ord, Read)

instance Supply (Compiler2State a) where
  updateSupply = overCompiler2Supply
  getSupply = compiler2Supply

{-# INLINE overCompiler2NameStore #-}
overCompiler2NameStore :: Over (Compiler2State a) (Environment (Scheme TypeIndex Kind IndexedType))
overCompiler2NameStore fn Compiler2State{..} = Compiler2State{compiler2NameStore = fn compiler2NameStore, ..}

{-# INLINE overCompiler2Supply #-}
overCompiler2Supply :: Over (Compiler2State a) Int
overCompiler2Supply fn Compiler2State{..} = Compiler2State{compiler2Supply = fn compiler2Supply, ..}

{-# INLINE overCompiler2Substitution #-}
overCompiler2Substitution :: Over (Compiler2State a) Substitution
overCompiler2Substitution fn Compiler2State{..} = Compiler2State{compiler2Substitution = fn compiler2Substitution, ..}

{-# INLINE overCompiler2Constraints #-}
overCompiler2Constraints :: Over (Compiler2State a) [CompilerConstraint a]
overCompiler2Constraints fn Compiler2State{..} = Compiler2State{compiler2Constraints = fn compiler2Constraints, ..}

{-# INLINE overCompiler2Assumptions #-}
overCompiler2Assumptions :: Over (Compiler2State a) [CompilerAssumption]
overCompiler2Assumptions fn Compiler2State{..} = Compiler2State{compiler2Assumptions = fn compiler2Assumptions, ..}

{-# INLINE overCompiler2StateConstraintsGenErrors #-}
overCompiler2StateConstraintsGenErrors :: Over (Compiler2State a) [ConstraintsGenError a]
overCompiler2StateConstraintsGenErrors fn Compiler2State{..} = Compiler2State{compiler2ConstraintsGenErrors = fn compiler2ConstraintsGenErrors, ..}

{-# INLINE overCompiler2TypeAnnotationParams #-}
overCompiler2TypeAnnotationParams :: Over (Compiler2State a) (Dictionary (a, TypeIndex Kind))
overCompiler2TypeAnnotationParams fn Compiler2State{..} = Compiler2State{compiler2TypeAnnotationParams = fn compiler2TypeAnnotationParams, ..}

{-# INLINE overCompiler2SolverRuleViolations #-}
overCompiler2SolverRuleViolations :: Over (Compiler2State a) [InferenceRule Kind a]
overCompiler2SolverRuleViolations fn Compiler2State{..} = Compiler2State{compiler2SolverRuleViolations = fn compiler2SolverRuleViolations, ..}

initialCompiler2State :: Compiler2State a
initialCompiler2State =
  Compiler2State
    { compiler2Supply = 0
    , compiler2NameStore = mempty
    , compiler2Substitution = mempty
    , compiler2Constraints = []
    , compiler2ConstraintsGenErrors = []
    , compiler2SolverRuleViolations = []
    , compiler2Assumptions = []
    , compiler2TypeAnnotationParams = mempty
    }

type Compiler2Stack a m c = RWST (Compiler2Environment TypeIndex Kind IndexedType) () (Compiler2State a) m c

newtype Compiler2T a m c = Compiler2 {compiler2Stack :: Compiler2Stack a m c}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (Compiler2Environment TypeIndex Kind IndexedType)
    , MonadState (Compiler2State a)
    )

{-# INLINE runCompiler2T #-}
runCompiler2T :: (Monad m) => Compiler2Environment TypeIndex Kind IndexedType -> Compiler2T a m c -> m (c, Compiler2State a)
runCompiler2T env com = do
  (c, s, _) <- runRWST (compiler2Stack com) env initialCompiler2State
  pure (c, s)

{-# INLINE evalCompiler2T #-}
evalCompiler2T :: (Monad m) => Compiler2Environment TypeIndex Kind IndexedType -> Compiler2T a m c -> m c
evalCompiler2T = fst <$$$> runCompiler2T

{-# INLINE compiler2ReportConstraintsGenErrors #-}
compiler2ReportConstraintsGenErrors :: (Monad m) => [ConstraintsGenError a] -> Compiler2T a m ()
compiler2ReportConstraintsGenErrors errors = modify (overCompiler2StateConstraintsGenErrors (<> errors))

{-# INLINE compiler2SetTypeAnnotationParams #-}
compiler2SetTypeAnnotationParams :: (Monad m) => Dictionary (a, TypeIndex Kind) -> Compiler2T a m ()
compiler2SetTypeAnnotationParams params = modify (overCompiler2TypeAnnotationParams (const params))

{-# INLINE compiler2ReportSolverRuleViolations #-}
compiler2ReportSolverRuleViolations :: (Monad m) => [InferenceRule Kind a] -> Compiler2T a m ()
compiler2ReportSolverRuleViolations errors = modify (overCompiler2SolverRuleViolations (<> errors))

{-# INLINE insertSupplyC #-}
insertSupplyC :: (Monad m) => Int -> Compiler2T a m ()
insertSupplyC = modify . overCompiler2Supply . const

{-# INLINE insertNameC #-}
insertNameC :: (Monad m) => Name -> Scheme TypeIndex Kind IndexedType -> Compiler2T a m ()
insertNameC name scheme = modify (overCompiler2NameStore (Environment.insert name scheme))

{-# INLINE insertNamesC #-}
insertNamesC :: (Monad m) => [(Name, Scheme TypeIndex Kind IndexedType)] -> Compiler2T a m ()
insertNamesC names = modify (overCompiler2NameStore (Environment.insertMultiple names))

{-# INLINE insertConstraintsC #-}
insertConstraintsC :: (Monad m) => [CompilerConstraint a] -> Compiler2T a m ()
insertConstraintsC cs = modify (overCompiler2Constraints (<> cs))

{-# INLINE clearConstraintsC #-}
clearConstraintsC :: (Monad m) => Compiler2T a m ()
clearConstraintsC = modify (overCompiler2Constraints (const mempty))

{-# INLINE insertAssumptionsC #-}
insertAssumptionsC :: (Monad m) => [CompilerAssumption] -> Compiler2T a m ()
insertAssumptionsC as = modify (overCompiler2Assumptions (<> as))

{-# INLINE updateSupplyC #-}
updateSupplyC :: (Monad m) => Int -> Compiler2T a m ()
updateSupplyC supply = modify (overCompiler2Supply (const supply))

{-# INLINE updateSubstitutionC #-}
updateSubstitutionC :: (Monad m) => Substitution -> Compiler2T a m ()
updateSubstitutionC sub = modify (overCompiler2Substitution (const sub))
