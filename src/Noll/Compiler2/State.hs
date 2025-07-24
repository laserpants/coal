{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler2.State (
  Compiler2State (..),
  CompilerAssumption,
  CompilerConstraint,
  overCompiler2Substitution,
  overCompiler2Supply,
  overCompiler2Assumptions,
  overCompiler2Constraints,
  overCompiler2NameStore,
  overCompiler2SolverRuleViolations,
  overCompiler2TypeAnnotationParams,
  overCompiler2StateConstraintsGenErrors,
  initialCompiler2State,
) where

import Lang.Common.Environment (Environment (..))
import Lang.Common.Supply (Supply (..))
import Lang.Utils (Dictionary, Over)
import Noll.Language
import Noll.TypeSystem

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
