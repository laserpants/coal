{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoState (
  ProtoCompilerState (..),
  initialProtoCompilerState,
  overProtoCompilerSupply,
  overProtoCompilerModules,
  overProtoCompilerCurrentPath,
) where

import Coal.Common.Environment (Environment (..))
import Coal.Common.Supply (Supply (..))
import Coal.Language.Module.Path (Path (..), emptyPath)
import Coal.ProtoCompiler.ProtoBuild (ProtoBuild (..))
import Extras (Over)

data ProtoCompilerState a = ProtoCompilerState
  { protoOcompilerSupply :: Int
  , protoOcompilerModules :: Environment (ProtoBuild a)
  , protoOcompilerCurrentPath :: Path
  }
  deriving (Show, Eq, Ord)

initialProtoCompilerState :: ProtoCompilerState a
initialProtoCompilerState =
  ProtoCompilerState
    { protoOcompilerSupply = 0
    , protoOcompilerModules = mempty
    , protoOcompilerCurrentPath = emptyPath
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

instance Supply (ProtoCompilerState a) where
  updateSupply = overProtoCompilerSupply
  getSupply = protoOcompilerSupply
