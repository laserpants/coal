{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoBuild (ProtoBuild (..), proto_emptyBuild) where

import Coal.Compiler.Build.Hash256 (Hash256 (..))
import Coal.Language.Module.Path (Path (..))
import Data.Binary
import Data.ByteString (ByteString)
import GHC.Generics (Generic)

data ProtoBuild = ProtoBuild
  { proto_buildPath :: Path
  , proto_buildFile :: FilePath
  , proto_buildBitcode :: Maybe ByteString
  , proto_buildHash :: Maybe Hash256
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary ProtoBuild

proto_emptyBuild :: ProtoBuild
proto_emptyBuild =
  ProtoBuild
    { proto_buildPath = Path []
    , proto_buildFile = mempty
    , proto_buildBitcode = Nothing
    , proto_buildHash = Nothing
    }

setBuildPath :: Path -> ProtoBuild -> ProtoBuild
setBuildPath newBuildPath ProtoBuild{..} =
  ProtoBuild
    { proto_buildPath = newBuildPath
    , ..
    }

setBuildFile :: FilePath -> ProtoBuild -> ProtoBuild
setBuildFile newBuildFile ProtoBuild{..} =
  ProtoBuild
    { proto_buildFile = newBuildFile
    , ..
    }

setBuildBitcode :: ByteString -> ProtoBuild -> ProtoBuild
setBuildBitcode newBuildBitcode ProtoBuild{..} =
  ProtoBuild
    { proto_buildBitcode = Just newBuildBitcode
    , ..
    }

setBuildHash :: Hash256 -> ProtoBuild -> ProtoBuild
setBuildHash newBuildHash ProtoBuild{..} =
  ProtoBuild
    { proto_buildHash = Just newBuildHash
    , ..
    }
