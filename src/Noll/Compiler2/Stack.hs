{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler2.Stack (
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
import Lang.Common.Supply (Supply (..))
import Lang.Utils (Dictionary, Name, (<$$$>))
import Noll.Compiler2.Environment (Compiler2Environment (..))
import Noll.Compiler2.State
import Noll.Language
import Noll.TypeSystem

import qualified Lang.Common.Environment as Environment

type Compiler2Stack a m c = RWST Compiler2Environment () (Compiler2State a) m c

newtype Compiler2T a m c = Compiler2 {compiler2Stack :: Compiler2Stack a m c}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Compiler2Environment
    , MonadState (Compiler2State a)
    )

{-# INLINE runCompiler2T #-}
runCompiler2T :: (Monad m) => Compiler2Environment -> Compiler2T a m c -> m (c, Compiler2State a)
runCompiler2T env com = do
  (c, s, _) <- runRWST (compiler2Stack com) env initialCompiler2State
  pure (c, s)

{-# INLINE evalCompiler2T #-}
evalCompiler2T :: (Monad m) => Compiler2Environment -> Compiler2T a m c -> m c
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
