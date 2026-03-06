{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoState (
  ProtoCompilerState (..),
  CompilerConstraint,
  CompilerAssumption,
  initialProtoCompilerState,
  overProtoCompilerSupply,
  overProtoCompilerModules,
  overProtoCompilerModuleWithPath,
  overProtoCompilerCurrentPath,
  overProtoCompilerSubstitution,
  overProtoCompilerNameStore,
  overProtoCompilerConstraints,
  overProtoCompilerKindConstraints,
  overProtoCompilerAssumptions,
  overProtoCompilerTypeAnnotationParams,
  overProtoCompilerConstraintsGenErrors,
  overProtoCompilerKindConstraintsGenErrors,
  overProtoCompilerSolverRuleViolations,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Common.Supply (Supply (..))
import Coal.Language
import Coal.Language.Module.Path (Path (..), emptyPath, principalPath)
import Coal.ProtoCompiler.ProtoBuild (ProtoBuild (..))
import Coal.ProtoTypeSystem.Kind.Constraint (ProtoKindConstraint (..))
import Coal.ProtoTypeSystem.Kind.Error (ProtoKindError (..))
import Coal.TypeSystem.Constraint
import Coal.TypeSystem.Constraint.Assumption
import Coal.TypeSystem.Constraint.Generation.Error (ConstraintsGenError (..))
import Coal.TypeSystem.Constraint.Generation.InferenceRule
import Coal.TypeSystem.Substitution
import Extras (Dictionary, Over)

type CompilerConstraint a = Constraint (InferenceRule Kind a) TypeIndex Kind IndexedType

type CompilerAssumption a = Assumption a IndexedType

data ProtoCompilerState a = ProtoCompilerState
  { protoOcompilerSupply :: Int
  , protoOcompilerModules :: Environment (ProtoBuild a)
  , protoOcompilerCurrentPath :: Path
  , protoOcompilerSubstitution :: Substitution
  , protoOcompilerNameStore :: Environment IndexedScheme
  , protoOcompilerConstraints :: [CompilerConstraint a]
  , protoOcompilerAssumptions :: [CompilerAssumption a]
  , protoOcompilerTypeAnnotationParams :: Dictionary (a, TypeIndex Kind)
  , protoOcompilerKindConstraints :: [ProtoKindConstraint]
  , protoOcompilerConstraintsGenErrors :: [ConstraintsGenError a]
  , protoOcompilerKindConstraintsGenErrors :: [ProtoKindError]
  , protoOcompilerSolverRuleViolations :: [InferenceRule Kind a]
  }
  deriving (Show, Eq, Ord)

initialProtoCompilerState :: ProtoCompilerState a
initialProtoCompilerState =
  ProtoCompilerState
    { protoOcompilerSupply = 0
    , protoOcompilerModules = mempty
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

{-# INLINE overProtoCompilerSupply #-}
overProtoCompilerSupply :: Over (ProtoCompilerState a) Int
overProtoCompilerSupply fn ProtoCompilerState{..} =
  ProtoCompilerState
    { protoOcompilerSupply = fn protoOcompilerSupply
    , ..
    }

{-# INLINE overProtoCompilerModules #-}
overProtoCompilerModules :: Over (ProtoCompilerState a) (Environment (ProtoBuild a))
overProtoCompilerModules fn ProtoCompilerState{..} =
  ProtoCompilerState
    { protoOcompilerModules = fn protoOcompilerModules
    , ..
    }

{-# INLINE overProtoCompilerModuleWithPath #-}
overProtoCompilerModuleWithPath :: Path -> Over (ProtoCompilerState a) (ProtoBuild a)
overProtoCompilerModuleWithPath path fn ProtoCompilerState{..} =
  ProtoCompilerState
    { protoOcompilerModules = Environment.adjust fn (principalPath path) protoOcompilerModules
    , ..
    }

{-# INLINE overProtoCompilerCurrentPath #-}
overProtoCompilerCurrentPath :: Over (ProtoCompilerState a) Path
overProtoCompilerCurrentPath fn ProtoCompilerState{..} =
  ProtoCompilerState
    { protoOcompilerCurrentPath = fn protoOcompilerCurrentPath
    , ..
    }

{-# INLINE overProtoCompilerSubstitution #-}
overProtoCompilerSubstitution :: Over (ProtoCompilerState a) Substitution
overProtoCompilerSubstitution fn ProtoCompilerState{..} =
  ProtoCompilerState
    { protoOcompilerSubstitution = fn protoOcompilerSubstitution
    , ..
    }

{-# INLINE overProtoCompilerNameStore #-}
overProtoCompilerNameStore :: Over (ProtoCompilerState a) (Environment IndexedScheme)
overProtoCompilerNameStore fn ProtoCompilerState{..} =
  ProtoCompilerState
    { protoOcompilerNameStore = fn protoOcompilerNameStore
    , ..
    }

{-# INLINE overProtoCompilerConstraints #-}
overProtoCompilerConstraints :: Over (ProtoCompilerState a) [CompilerConstraint a]
overProtoCompilerConstraints fn ProtoCompilerState{..} =
  ProtoCompilerState
    { protoOcompilerConstraints = fn protoOcompilerConstraints
    , ..
    }

{-# INLINE overProtoCompilerKindConstraints #-}
overProtoCompilerKindConstraints :: Over (ProtoCompilerState a) [ProtoKindConstraint]
overProtoCompilerKindConstraints fn ProtoCompilerState{..} =
  ProtoCompilerState
    { protoOcompilerKindConstraints = fn protoOcompilerKindConstraints
    , ..
    }

{-# INLINE overProtoCompilerAssumptions #-}
overProtoCompilerAssumptions :: Over (ProtoCompilerState a) [CompilerAssumption a]
overProtoCompilerAssumptions fn ProtoCompilerState{..} =
  ProtoCompilerState
    { protoOcompilerAssumptions = fn protoOcompilerAssumptions
    , ..
    }

{-# INLINE overProtoCompilerTypeAnnotationParams #-}
overProtoCompilerTypeAnnotationParams :: Over (ProtoCompilerState a) (Dictionary (a, TypeIndex Kind))
overProtoCompilerTypeAnnotationParams fn ProtoCompilerState{..} =
  ProtoCompilerState
    { protoOcompilerTypeAnnotationParams = fn protoOcompilerTypeAnnotationParams
    , ..
    }

{-# INLINE overProtoCompilerConstraintsGenErrors #-}
overProtoCompilerConstraintsGenErrors :: Over (ProtoCompilerState a) [ConstraintsGenError a]
overProtoCompilerConstraintsGenErrors fn ProtoCompilerState{..} =
  ProtoCompilerState
    { protoOcompilerConstraintsGenErrors = fn protoOcompilerConstraintsGenErrors
    , ..
    }

{-# INLINE overProtoCompilerKindConstraintsGenErrors #-}
overProtoCompilerKindConstraintsGenErrors :: Over (ProtoCompilerState a) [ProtoKindError]
overProtoCompilerKindConstraintsGenErrors fn ProtoCompilerState{..} =
  ProtoCompilerState
    { protoOcompilerKindConstraintsGenErrors = fn protoOcompilerKindConstraintsGenErrors
    , ..
    }

{-# INLINE overProtoCompilerSolverRuleViolations #-}
overProtoCompilerSolverRuleViolations :: Over (ProtoCompilerState a) [InferenceRule Kind a]
overProtoCompilerSolverRuleViolations fn ProtoCompilerState{..} =
  ProtoCompilerState
    { protoOcompilerSolverRuleViolations = fn protoOcompilerSolverRuleViolations
    , ..
    }

instance Supply (ProtoCompilerState a) where
  updateSupply = overProtoCompilerSupply
  getSupply = protoOcompilerSupply
