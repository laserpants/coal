{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoState (
  ProtoCompilerState (..),
  initialProtoCompilerState,
  overProtoCompilerSupply,
  overProtoCompilerModules,
  overProtoCompilerCurrentPath,
  overProtoCompilerConstraints,
  overProtoCompilerAssumptions,
) where

import Coal.Common.Environment (Environment (..))
import Coal.Common.Supply (Supply (..))
import Coal.Language
import Coal.Language.Module.Path (Path (..), emptyPath)
import Coal.ProtoCompiler.ProtoBuild (ProtoBuild (..))
import Coal.TypeSystem.Constraint
import Coal.TypeSystem.Constraint.Assumption
import Coal.TypeSystem.Constraint.Generation.InferenceRule
import Extras (Over)

type CompilerConstraint a = Constraint (InferenceRule Kind a) TypeIndex Kind IndexedType

type CompilerAssumption a = Assumption a IndexedType

data ProtoCompilerState a = ProtoCompilerState
  { protoOcompilerSupply :: Int
  , protoOcompilerModules :: Environment (ProtoBuild a)
  , protoOcompilerCurrentPath :: Path
  , protoOcompilerConstraints :: [CompilerConstraint a]
  , protoOcompilerAssumptions :: [CompilerAssumption a]
  }
  deriving (Show, Eq, Ord)

initialProtoCompilerState :: ProtoCompilerState a
initialProtoCompilerState =
  ProtoCompilerState
    { protoOcompilerSupply = 0
    , protoOcompilerModules = mempty
    , protoOcompilerCurrentPath = emptyPath
    , protoOcompilerConstraints = mempty
    , protoOcompilerAssumptions = mempty
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

instance Supply (ProtoCompilerState a) where
  updateSupply = overProtoCompilerSupply
  getSupply = protoOcompilerSupply
