{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoState (
  ProtoCompilerState (..),
  CompilerConstraint,
  CompilerAssumption,
  initialProtoCompilerState,
  overProtoCompilerSupply,
  overProtoCompilerModules,
  overProtoCompilerCurrentPath,
  overProtoCompilerSubstitution,
  overProtoCompilerNameStore,
  overProtoCompilerConstraints,
  overProtoCompilerAssumptions,
  overProtoCompilerTypeAnnotationParams,
) where

import Coal.Common.Environment (Environment (..))
import Coal.Common.Supply (Supply (..))
import Coal.Language
import Coal.Language.Module.Path (Path (..), emptyPath)
import Coal.ProtoCompiler.ProtoBuild (ProtoBuild (..))
import Coal.TypeSystem.Constraint
import Coal.TypeSystem.Constraint.Assumption
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
  , -- TODO: add kind constraints
    protoOcompilerAssumptions :: [CompilerAssumption a]
  , protoOcompilerTypeAnnotationParams :: Dictionary (a, TypeIndex Kind)
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

instance Supply (ProtoCompilerState a) where
  updateSupply = overProtoCompilerSupply
  getSupply = protoOcompilerSupply
