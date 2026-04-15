-- +
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.State (
  CompilerState (..),
  CompilerConstraint,
  CompilerAssumption,
  initialCompilerState,
  overCompilerSupply,
  overCompilerConfig,
  overCompilerModules,
  overCompilerSources,
  overCompilerTouched,
  overCompilerModuleWithPath,
  overCompilerCurrentPath,
  overCompilerSubstitution,
  overCompilerNameStore,
  overCompilerConstraints,
  overCompilerKindConstraints,
  overCompilerAssumptions,
  overCompilerTypeAnnotationParams,
  overCompilerConstraintsGenErrors,
  overCompilerKindConstraintsGenErrors,
  overCompilerSolverRuleViolations,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Common.Supply (Supply (..))
import Coal.Compiler.Build (Build (..))
import Coal.Compiler.Config (CompilerConfig (..), defaultConfig)
import Coal.Language
import Coal.Language.Module.Path (Path (..), emptyPath, principalPath)
import Coal.TypeSystem.Constraint
import Coal.TypeSystem.Constraint.Assumption
import Coal.TypeSystem.Constraint.Generation.Error (ConstraintsGenError (..))
import Coal.TypeSystem.Constraint.Generation.InferenceRule
import Coal.TypeSystem.Kind.Constraint (KindConstraint (..))
import Coal.TypeSystem.Kind.Error (KindError (..))
import Coal.TypeSystem.Substitution
import Data.Set (Set)
import Data.Text (Text)
import Extras (Dictionary, Name, Over)

type CompilerConstraint a = Constraint (InferenceRule Kind a) TypeIndex Kind IndexedType

type CompilerAssumption a = Assumption a IndexedType

data CompilerState a = CompilerState
  { compilerSupply :: Int
  , compilerConfig :: CompilerConfig
  , compilerModules :: Environment (Build a)
  , compilerSources :: Environment Text
  , compilerTouched :: Set Name
  , compilerCurrentPath :: Path
  , compilerSubstitution :: Substitution
  , compilerNameStore :: Environment IndexedScheme
  , compilerConstraints :: [CompilerConstraint a]
  , compilerAssumptions :: [CompilerAssumption a]
  , compilerTypeAnnotationParams :: Dictionary (a, TypeIndex Kind)
  , compilerKindConstraints :: [KindConstraint]
  , compilerConstraintsGenErrors :: [ConstraintsGenError a]
  , compilerKindConstraintsGenErrors :: [KindError]
  , compilerSolverRuleViolations :: [InferenceRule Kind a]
  }
  deriving (Show, Eq, Ord)

instance Supply (CompilerState a) where
  updateSupply = overCompilerSupply
  getSupply = compilerSupply

initialCompilerState :: CompilerState a
initialCompilerState =
  CompilerState
    { compilerSupply = 0
    , compilerConfig = defaultConfig
    , compilerModules = mempty
    , compilerSources = mempty
    , compilerTouched = mempty
    , compilerCurrentPath = emptyPath
    , compilerSubstitution = mempty
    , compilerNameStore = mempty
    , compilerConstraints = mempty
    , compilerAssumptions = mempty
    , compilerTypeAnnotationParams = mempty
    , compilerKindConstraints = mempty
    , compilerConstraintsGenErrors = []
    , compilerKindConstraintsGenErrors = []
    , compilerSolverRuleViolations = []
    }

{-# INLINE overCompilerSupply #-}
overCompilerSupply :: Over (CompilerState a) Int
overCompilerSupply fn CompilerState{..} =
  CompilerState
    { compilerSupply = fn compilerSupply
    , ..
    }

{-# INLINE overCompilerConfig #-}
overCompilerConfig :: Over (CompilerState a) CompilerConfig
overCompilerConfig fn CompilerState{..} =
  CompilerState
    { compilerConfig = fn compilerConfig
    , ..
    }

{-# INLINE overCompilerModules #-}
overCompilerModules :: Over (CompilerState a) (Environment (Build a))
overCompilerModules fn CompilerState{..} =
  CompilerState
    { compilerModules = fn compilerModules
    , ..
    }

{-# INLINE overCompilerModuleWithPath #-}
overCompilerModuleWithPath :: Path -> Over (CompilerState a) (Build a)
overCompilerModuleWithPath path fn CompilerState{..} =
  CompilerState
    { compilerModules = Environment.adjust fn (principalPath path) compilerModules
    , ..
    }

{-# INLINE overCompilerSources #-}
overCompilerSources :: Over (CompilerState a) (Environment Text)
overCompilerSources fn CompilerState{..} =
  CompilerState
    { compilerSources = fn compilerSources
    , ..
    }

{-# INLINE overCompilerTouched #-}
overCompilerTouched :: Over (CompilerState a) (Set Name)
overCompilerTouched fn CompilerState{..} =
  CompilerState
    { compilerTouched = fn compilerTouched
    , ..
    }

{-# INLINE overCompilerCurrentPath #-}
overCompilerCurrentPath :: Over (CompilerState a) Path
overCompilerCurrentPath fn CompilerState{..} =
  CompilerState
    { compilerCurrentPath = fn compilerCurrentPath
    , ..
    }

{-# INLINE overCompilerSubstitution #-}
overCompilerSubstitution :: Over (CompilerState a) Substitution
overCompilerSubstitution fn CompilerState{..} =
  CompilerState
    { compilerSubstitution = fn compilerSubstitution
    , ..
    }

{-# INLINE overCompilerNameStore #-}
overCompilerNameStore :: Over (CompilerState a) (Environment IndexedScheme)
overCompilerNameStore fn CompilerState{..} =
  CompilerState
    { compilerNameStore = fn compilerNameStore
    , ..
    }

{-# INLINE overCompilerConstraints #-}
overCompilerConstraints :: Over (CompilerState a) [CompilerConstraint a]
overCompilerConstraints fn CompilerState{..} =
  CompilerState
    { compilerConstraints = fn compilerConstraints
    , ..
    }

{-# INLINE overCompilerKindConstraints #-}
overCompilerKindConstraints :: Over (CompilerState a) [KindConstraint]
overCompilerKindConstraints fn CompilerState{..} =
  CompilerState
    { compilerKindConstraints = fn compilerKindConstraints
    , ..
    }

{-# INLINE overCompilerAssumptions #-}
overCompilerAssumptions :: Over (CompilerState a) [CompilerAssumption a]
overCompilerAssumptions fn CompilerState{..} =
  CompilerState
    { compilerAssumptions = fn compilerAssumptions
    , ..
    }

{-# INLINE overCompilerTypeAnnotationParams #-}
overCompilerTypeAnnotationParams :: Over (CompilerState a) (Dictionary (a, TypeIndex Kind))
overCompilerTypeAnnotationParams fn CompilerState{..} =
  CompilerState
    { compilerTypeAnnotationParams = fn compilerTypeAnnotationParams
    , ..
    }

{-# INLINE overCompilerConstraintsGenErrors #-}
overCompilerConstraintsGenErrors :: Over (CompilerState a) [ConstraintsGenError a]
overCompilerConstraintsGenErrors fn CompilerState{..} =
  CompilerState
    { compilerConstraintsGenErrors = fn compilerConstraintsGenErrors
    , ..
    }

{-# INLINE overCompilerKindConstraintsGenErrors #-}
overCompilerKindConstraintsGenErrors :: Over (CompilerState a) [KindError]
overCompilerKindConstraintsGenErrors fn CompilerState{..} =
  CompilerState
    { compilerKindConstraintsGenErrors = fn compilerKindConstraintsGenErrors
    , ..
    }

{-# INLINE overCompilerSolverRuleViolations #-}
overCompilerSolverRuleViolations :: Over (CompilerState a) [InferenceRule Kind a]
overCompilerSolverRuleViolations fn CompilerState{..} =
  CompilerState
    { compilerSolverRuleViolations = fn compilerSolverRuleViolations
    , ..
    }
