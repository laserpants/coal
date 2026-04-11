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
  overCompilerToBeRecompiled,
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
  { protoOcompilerSupply :: Int
  , protoOcompilerConfig :: CompilerConfig
  , protoOcompilerModules :: Environment (Build a)
  , protoOcompilerSources :: Environment Text
  , protoOcompilerToBeRecompiled :: Set Name
  , protoOcompilerCurrentPath :: Path
  , protoOcompilerSubstitution :: Substitution
  , protoOcompilerNameStore :: Environment IndexedScheme
  , protoOcompilerConstraints :: [CompilerConstraint a]
  , protoOcompilerAssumptions :: [CompilerAssumption a]
  , protoOcompilerTypeAnnotationParams :: Dictionary (a, TypeIndex Kind)
  , protoOcompilerKindConstraints :: [KindConstraint]
  , protoOcompilerConstraintsGenErrors :: [ConstraintsGenError a]
  , protoOcompilerKindConstraintsGenErrors :: [KindError]
  , protoOcompilerSolverRuleViolations :: [InferenceRule Kind a]
  }
  deriving (Show, Eq, Ord)

instance Supply (CompilerState a) where
  updateSupply = overCompilerSupply
  getSupply = protoOcompilerSupply

initialCompilerState :: CompilerState a
initialCompilerState =
  CompilerState
    { protoOcompilerSupply = 0
    , protoOcompilerConfig = defaultConfig
    , protoOcompilerModules = mempty
    , protoOcompilerSources = mempty
    , protoOcompilerToBeRecompiled = mempty
    , protoOcompilerCurrentPath = emptyPath
    , protoOcompilerSubstitution = mempty
    , protoOcompilerNameStore = mempty
    , protoOcompilerConstraints = mempty
    , protoOcompilerAssumptions = mempty
    , protoOcompilerTypeAnnotationParams = mempty
    , protoOcompilerKindConstraints = mempty
    , protoOcompilerConstraintsGenErrors = []
    , protoOcompilerKindConstraintsGenErrors = []
    , protoOcompilerSolverRuleViolations = []
    }

{-# INLINE overCompilerSupply #-}
overCompilerSupply :: Over (CompilerState a) Int
overCompilerSupply fn CompilerState{..} =
  CompilerState
    { protoOcompilerSupply = fn protoOcompilerSupply
    , ..
    }

{-# INLINE overCompilerConfig #-}
overCompilerConfig :: Over (CompilerState a) CompilerConfig
overCompilerConfig fn CompilerState{..} =
  CompilerState
    { protoOcompilerConfig = fn protoOcompilerConfig
    , ..
    }

{-# INLINE overCompilerModules #-}
overCompilerModules :: Over (CompilerState a) (Environment (Build a))
overCompilerModules fn CompilerState{..} =
  CompilerState
    { protoOcompilerModules = fn protoOcompilerModules
    , ..
    }

{-# INLINE overCompilerModuleWithPath #-}
overCompilerModuleWithPath :: Path -> Over (CompilerState a) (Build a)
overCompilerModuleWithPath path fn CompilerState{..} =
  CompilerState
    { protoOcompilerModules = Environment.adjust fn (principalPath path) protoOcompilerModules
    , ..
    }

{-# INLINE overCompilerSources #-}
overCompilerSources :: Over (CompilerState a) (Environment Text)
overCompilerSources fn CompilerState{..} =
  CompilerState
    { protoOcompilerSources = fn protoOcompilerSources
    , ..
    }

{-# INLINE overCompilerToBeRecompiled #-}
overCompilerToBeRecompiled :: Over (CompilerState a) (Set Name)
overCompilerToBeRecompiled fn CompilerState{..} =
  CompilerState
    { protoOcompilerToBeRecompiled = fn protoOcompilerToBeRecompiled
    , ..
    }

{-# INLINE overCompilerCurrentPath #-}
overCompilerCurrentPath :: Over (CompilerState a) Path
overCompilerCurrentPath fn CompilerState{..} =
  CompilerState
    { protoOcompilerCurrentPath = fn protoOcompilerCurrentPath
    , ..
    }

{-# INLINE overCompilerSubstitution #-}
overCompilerSubstitution :: Over (CompilerState a) Substitution
overCompilerSubstitution fn CompilerState{..} =
  CompilerState
    { protoOcompilerSubstitution = fn protoOcompilerSubstitution
    , ..
    }

{-# INLINE overCompilerNameStore #-}
overCompilerNameStore :: Over (CompilerState a) (Environment IndexedScheme)
overCompilerNameStore fn CompilerState{..} =
  CompilerState
    { protoOcompilerNameStore = fn protoOcompilerNameStore
    , ..
    }

{-# INLINE overCompilerConstraints #-}
overCompilerConstraints :: Over (CompilerState a) [CompilerConstraint a]
overCompilerConstraints fn CompilerState{..} =
  CompilerState
    { protoOcompilerConstraints = fn protoOcompilerConstraints
    , ..
    }

{-# INLINE overCompilerKindConstraints #-}
overCompilerKindConstraints :: Over (CompilerState a) [KindConstraint]
overCompilerKindConstraints fn CompilerState{..} =
  CompilerState
    { protoOcompilerKindConstraints = fn protoOcompilerKindConstraints
    , ..
    }

{-# INLINE overCompilerAssumptions #-}
overCompilerAssumptions :: Over (CompilerState a) [CompilerAssumption a]
overCompilerAssumptions fn CompilerState{..} =
  CompilerState
    { protoOcompilerAssumptions = fn protoOcompilerAssumptions
    , ..
    }

{-# INLINE overCompilerTypeAnnotationParams #-}
overCompilerTypeAnnotationParams :: Over (CompilerState a) (Dictionary (a, TypeIndex Kind))
overCompilerTypeAnnotationParams fn CompilerState{..} =
  CompilerState
    { protoOcompilerTypeAnnotationParams = fn protoOcompilerTypeAnnotationParams
    , ..
    }

{-# INLINE overCompilerConstraintsGenErrors #-}
overCompilerConstraintsGenErrors :: Over (CompilerState a) [ConstraintsGenError a]
overCompilerConstraintsGenErrors fn CompilerState{..} =
  CompilerState
    { protoOcompilerConstraintsGenErrors = fn protoOcompilerConstraintsGenErrors
    , ..
    }

{-# INLINE overCompilerKindConstraintsGenErrors #-}
overCompilerKindConstraintsGenErrors :: Over (CompilerState a) [KindError]
overCompilerKindConstraintsGenErrors fn CompilerState{..} =
  CompilerState
    { protoOcompilerKindConstraintsGenErrors = fn protoOcompilerKindConstraintsGenErrors
    , ..
    }

{-# INLINE overCompilerSolverRuleViolations #-}
overCompilerSolverRuleViolations :: Over (CompilerState a) [InferenceRule Kind a]
overCompilerSolverRuleViolations fn CompilerState{..} =
  CompilerState
    { protoOcompilerSolverRuleViolations = fn protoOcompilerSolverRuleViolations
    , ..
    }
