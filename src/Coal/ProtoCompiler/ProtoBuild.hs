{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoBuild (
  ProtoBuild (..),
  protoOemptyBuild,
  setBuildPath,
  setBuildFile,
  setBuildBitcode,
  setBuildHash,
  setBuildKernelNames,
  setBuildKernelIRTypes,
  setBuildKernelConstructors,
  overBuildNames,
  insertBuildNameEntry,
  overBuildExportedNames,
  insertBuildExportedName,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build.Hash256 (Hash256 (..))
import Coal.Kernel.LLVM.IRType (IRType)
import qualified Coal.Kernel.Language as Kernel
import Coal.Language.Module.Path (Path (..))
import Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry (ProtoNameEntry (..))
import Data.Binary
import Data.ByteString (ByteString)
import qualified Data.Set as Set
import Extras (Name, Set)
import GHC.Generics (Generic)

data ProtoBuild = ProtoBuild
  { protoObuildPath :: Path
  , protoObuildFile :: FilePath
  , protoObuildNames :: Environment [ProtoNameEntry]
  , protoObuildExportedNames :: Set Name
  , protoObuildBitcode :: Maybe ByteString
  , protoObuildHash :: Maybe Hash256
  , protoObuildKernelNames :: Environment Kernel.Type
  , protoObuildKernelIRTypes :: Environment IRType
  , protoObuildKernelConstructors :: Environment Int
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary ProtoBuild

protoOemptyBuild :: ProtoBuild
protoOemptyBuild =
  ProtoBuild
    { protoObuildPath = Path []
    , protoObuildFile = mempty
    , protoObuildNames = mempty
    , protoObuildExportedNames = mempty
    , protoObuildBitcode = Nothing
    , protoObuildHash = Nothing
    , protoObuildKernelNames = mempty
    , protoObuildKernelIRTypes = mempty
    , protoObuildKernelConstructors = mempty
    }

setBuildPath :: Path -> ProtoBuild -> ProtoBuild
setBuildPath newBuildPath ProtoBuild{..} =
  ProtoBuild
    { protoObuildPath = newBuildPath
    , ..
    }

setBuildFile :: FilePath -> ProtoBuild -> ProtoBuild
setBuildFile newBuildFile ProtoBuild{..} =
  ProtoBuild
    { protoObuildFile = newBuildFile
    , ..
    }

overBuildNames :: (Environment [ProtoNameEntry] -> Environment [ProtoNameEntry]) -> ProtoBuild -> ProtoBuild
overBuildNames f ProtoBuild{..} =
  ProtoBuild
    { protoObuildNames = f protoObuildNames
    , ..
    }

insertBuildNameEntry :: Name -> ProtoNameEntry -> ProtoBuild -> ProtoBuild
insertBuildNameEntry name entry = overBuildNames (Environment.insertWith (<>) name [entry])

overBuildExportedNames :: (Set Name -> Set Name) -> ProtoBuild -> ProtoBuild
overBuildExportedNames f ProtoBuild{..} =
  ProtoBuild
    { protoObuildExportedNames = f protoObuildExportedNames
    , ..
    }

insertBuildExportedName :: Name -> ProtoBuild -> ProtoBuild
insertBuildExportedName name = overBuildExportedNames (Set.insert name)

setBuildBitcode :: ByteString -> ProtoBuild -> ProtoBuild
setBuildBitcode newBuildBitcode ProtoBuild{..} =
  ProtoBuild
    { protoObuildBitcode = Just newBuildBitcode
    , ..
    }

setBuildHash :: Hash256 -> ProtoBuild -> ProtoBuild
setBuildHash newBuildHash ProtoBuild{..} =
  ProtoBuild
    { protoObuildHash = Just newBuildHash
    , ..
    }

setBuildKernelNames :: Environment Kernel.Type -> ProtoBuild -> ProtoBuild
setBuildKernelNames env ProtoBuild{..} =
  ProtoBuild
    { protoObuildKernelNames = env
    , ..
    }

setBuildKernelIRTypes :: Environment IRType -> ProtoBuild -> ProtoBuild
setBuildKernelIRTypes env ProtoBuild{..} =
  ProtoBuild
    { protoObuildKernelIRTypes = env
    , ..
    }

setBuildKernelConstructors :: Environment Int -> ProtoBuild -> ProtoBuild
setBuildKernelConstructors env ProtoBuild{..} =
  ProtoBuild
    { protoObuildKernelConstructors = env
    , ..
    }
