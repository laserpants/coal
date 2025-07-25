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
  { compilerSupply :: Int
  , compilerNameStore :: Environment IndexedScheme
  , compilerSubstitution :: Substitution
  , compilerConstraints :: [CompilerConstraint a]
  , compilerConstraintsGenErrors :: [ConstraintsGenError a]
  , compilerSolverRuleViolations :: [InferenceRule Kind a]
  , compilerAssumptions :: [CompilerAssumption]
  , compilerTypeAnnotationParams :: Dictionary (a, TypeIndex Kind)
  }
  deriving (Show, Eq, Ord, Read)

instance Supply (CompilerState a) where
  updateSupply = overCompilerSupply
  getSupply = compilerSupply

{-# INLINE overCompilerNameStore #-}
overCompilerNameStore :: Over (CompilerState a) (Environment IndexedScheme)
overCompilerNameStore fn CompilerState{..} = CompilerState{compilerNameStore = fn compilerNameStore, ..}

{-# INLINE overCompilerSupply #-}
overCompilerSupply :: Over (CompilerState a) Int
overCompilerSupply fn CompilerState{..} = CompilerState{compilerSupply = fn compilerSupply, ..}

{-# INLINE overCompilerSubstitution #-}
overCompilerSubstitution :: Over (CompilerState a) Substitution
overCompilerSubstitution fn CompilerState{..} = CompilerState{compilerSubstitution = fn compilerSubstitution, ..}

{-# INLINE overCompilerConstraints #-}
overCompilerConstraints :: Over (CompilerState a) [CompilerConstraint a]
overCompilerConstraints fn CompilerState{..} = CompilerState{compilerConstraints = fn compilerConstraints, ..}

{-# INLINE overCompilerAssumptions #-}
overCompilerAssumptions :: Over (CompilerState a) [CompilerAssumption]
overCompilerAssumptions fn CompilerState{..} = CompilerState{compilerAssumptions = fn compilerAssumptions, ..}

{-# INLINE overCompilerStateConstraintsGenErrors #-}
overCompilerStateConstraintsGenErrors :: Over (CompilerState a) [ConstraintsGenError a]
overCompilerStateConstraintsGenErrors fn CompilerState{..} = CompilerState{compilerConstraintsGenErrors = fn compilerConstraintsGenErrors, ..}

{-# INLINE overCompilerTypeAnnotationParams #-}
overCompilerTypeAnnotationParams :: Over (CompilerState a) (Dictionary (a, TypeIndex Kind))
overCompilerTypeAnnotationParams fn CompilerState{..} = CompilerState{compilerTypeAnnotationParams = fn compilerTypeAnnotationParams, ..}

{-# INLINE overCompilerSolverRuleViolations #-}
overCompilerSolverRuleViolations :: Over (CompilerState a) [InferenceRule Kind a]
overCompilerSolverRuleViolations fn CompilerState{..} = CompilerState{compilerSolverRuleViolations = fn compilerSolverRuleViolations, ..}

initialCompilerState :: CompilerState a
initialCompilerState =
  CompilerState
    { compilerSupply = 0
    , compilerNameStore = mempty
    , compilerSubstitution = mempty
    , compilerConstraints = []
    , compilerConstraintsGenErrors = []
    , compilerSolverRuleViolations = []
    , compilerAssumptions = []
    , compilerTypeAnnotationParams = mempty
    }
