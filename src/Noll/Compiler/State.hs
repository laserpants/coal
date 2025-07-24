{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.State (
  CompilerState (..),
  CompilerAssumption,
  CompilerConstraint,
  overCompilerSubstitution,
  overCompilerSupply,
  overCompilerAssumptions,
  overCompilerConstraints,
  overCompilerNameStore,
  overCompilerSolverRuleViolations,
  overCompilerTypeAnnotationParams,
  overCompilerStateConstraintsGenErrors,
  initialCompilerState,
) where

import Lang.Common.Environment (Environment (..))
import Lang.Common.Supply (Supply (..))
import Lang.Utils (Dictionary, Over)
import Noll.Language
import Noll.TypeSystem

type CompilerConstraint a = Constraint (InferenceRule Kind a) TypeIndex Kind IndexedType

type CompilerAssumption = Assumption IndexedType

data CompilerState a = CompilerState
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

instance Supply (CompilerState a) where
  updateSupply = overCompilerSupply
  getSupply = compiler2Supply

{-# INLINE overCompilerNameStore #-}
overCompilerNameStore :: Over (CompilerState a) (Environment (Scheme TypeIndex Kind IndexedType))
overCompilerNameStore fn CompilerState{..} = CompilerState{compiler2NameStore = fn compiler2NameStore, ..}

{-# INLINE overCompilerSupply #-}
overCompilerSupply :: Over (CompilerState a) Int
overCompilerSupply fn CompilerState{..} = CompilerState{compiler2Supply = fn compiler2Supply, ..}

{-# INLINE overCompilerSubstitution #-}
overCompilerSubstitution :: Over (CompilerState a) Substitution
overCompilerSubstitution fn CompilerState{..} = CompilerState{compiler2Substitution = fn compiler2Substitution, ..}

{-# INLINE overCompilerConstraints #-}
overCompilerConstraints :: Over (CompilerState a) [CompilerConstraint a]
overCompilerConstraints fn CompilerState{..} = CompilerState{compiler2Constraints = fn compiler2Constraints, ..}

{-# INLINE overCompilerAssumptions #-}
overCompilerAssumptions :: Over (CompilerState a) [CompilerAssumption]
overCompilerAssumptions fn CompilerState{..} = CompilerState{compiler2Assumptions = fn compiler2Assumptions, ..}

{-# INLINE overCompilerStateConstraintsGenErrors #-}
overCompilerStateConstraintsGenErrors :: Over (CompilerState a) [ConstraintsGenError a]
overCompilerStateConstraintsGenErrors fn CompilerState{..} = CompilerState{compiler2ConstraintsGenErrors = fn compiler2ConstraintsGenErrors, ..}

{-# INLINE overCompilerTypeAnnotationParams #-}
overCompilerTypeAnnotationParams :: Over (CompilerState a) (Dictionary (a, TypeIndex Kind))
overCompilerTypeAnnotationParams fn CompilerState{..} = CompilerState{compiler2TypeAnnotationParams = fn compiler2TypeAnnotationParams, ..}

{-# INLINE overCompilerSolverRuleViolations #-}
overCompilerSolverRuleViolations :: Over (CompilerState a) [InferenceRule Kind a]
overCompilerSolverRuleViolations fn CompilerState{..} = CompilerState{compiler2SolverRuleViolations = fn compiler2SolverRuleViolations, ..}

initialCompilerState :: CompilerState a
initialCompilerState =
  CompilerState
    { compiler2Supply = 0
    , compiler2NameStore = mempty
    , compiler2Substitution = mempty
    , compiler2Constraints = []
    , compiler2ConstraintsGenErrors = []
    , compiler2SolverRuleViolations = []
    , compiler2Assumptions = []
    , compiler2TypeAnnotationParams = mempty
    }
