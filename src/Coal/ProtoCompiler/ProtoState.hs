{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoState (
  ProtoCompilerState (..),
  initialProtoCompilerState,
) where

import Coal.Common.Environment (Environment (..))
import Coal.Common.Supply (Supply (..))
import Coal.ProtoCompiler.ProtoBuild (ProtoBuild (..))
import Extras (Over)

data ProtoCompilerState a = ProtoCompilerState
  { protoOcompilerSupply :: Int
  , protoOcompilerModules :: Environment (ProtoBuild a)
  }
  deriving (Show, Eq, Ord)

initialProtoCompilerState :: ProtoCompilerState a
initialProtoCompilerState =
  ProtoCompilerState
    { protoOcompilerSupply = 0
    , protoOcompilerModules = mempty
    }

{-# INLINE overProtoCompilerSupply #-}
overProtoCompilerSupply :: Over (ProtoCompilerState a) Int
overProtoCompilerSupply fn ProtoCompilerState{..} =
  ProtoCompilerState
    { protoOcompilerSupply = fn protoOcompilerSupply
    , ..
    }

instance Supply (ProtoCompilerState a) where
  updateSupply = overProtoCompilerSupply
  getSupply = protoOcompilerSupply
