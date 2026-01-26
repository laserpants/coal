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
import Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry (ProtoDataConstructorEntry (..), ProtoNameEntry (..))
import Data.Binary
import Data.ByteString (ByteString)
import qualified Data.Set as Set
import Extras (Name, Set)
import GHC.Generics (Generic)

data ProtoBuild a = ProtoBuild
  { protoObuildPath :: Path
  , protoObuildFile :: FilePath
  , protoObuildNames :: Environment [ProtoNameEntry]
  , protoObuildExportedNames :: Set Name
  , protoObuildDataConstructors :: Environment (ProtoDataConstructorEntry a)
  , protoObuildBitcode :: Maybe ByteString
  , protoObuildHash :: Maybe Hash256
  , protoObuildKernelNames :: Environment Kernel.Type
  , protoObuildKernelIRTypes :: Environment IRType
  , protoObuildKernelConstructors :: Environment Int
  }
  deriving (Show, Eq, Ord, Generic)

instance (Binary a) => Binary (ProtoBuild a)

protoOemptyBuild :: ProtoBuild a
protoOemptyBuild =
  ProtoBuild
    { protoObuildPath = Path []
    , protoObuildFile = mempty
    , protoObuildNames = mempty
    , protoObuildExportedNames = mempty
    , protoObuildDataConstructors = mempty
    , protoObuildBitcode = Nothing
    , protoObuildHash = Nothing
    , protoObuildKernelNames = mempty
    , protoObuildKernelIRTypes = mempty
    , protoObuildKernelConstructors = mempty
    }

setBuildPath :: Path -> ProtoBuild a -> ProtoBuild a
setBuildPath newBuildPath ProtoBuild{..} =
  ProtoBuild
    { protoObuildPath = newBuildPath
    , ..
    }

setBuildFile :: FilePath -> ProtoBuild a -> ProtoBuild a
setBuildFile newBuildFile ProtoBuild{..} =
  ProtoBuild
    { protoObuildFile = newBuildFile
    , ..
    }

overBuildNames :: (Environment [ProtoNameEntry] -> Environment [ProtoNameEntry]) -> ProtoBuild a -> ProtoBuild a
overBuildNames f ProtoBuild{..} =
  ProtoBuild
    { protoObuildNames = f protoObuildNames
    , ..
    }

insertBuildNameEntry :: Name -> ProtoNameEntry -> ProtoBuild a -> ProtoBuild a
insertBuildNameEntry name entry = overBuildNames (Environment.insertWith (<>) name [entry])

overBuildExportedNames :: (Set Name -> Set Name) -> ProtoBuild a -> ProtoBuild a
overBuildExportedNames f ProtoBuild{..} =
  ProtoBuild
    { protoObuildExportedNames = f protoObuildExportedNames
    , ..
    }

insertBuildExportedName :: Name -> ProtoBuild a -> ProtoBuild a
insertBuildExportedName name = overBuildExportedNames (Set.insert name)

setBuildBitcode :: ByteString -> ProtoBuild a -> ProtoBuild a
setBuildBitcode newBuildBitcode ProtoBuild{..} =
  ProtoBuild
    { protoObuildBitcode = Just newBuildBitcode
    , ..
    }

setBuildHash :: Hash256 -> ProtoBuild a -> ProtoBuild a
setBuildHash newBuildHash ProtoBuild{..} =
  ProtoBuild
    { protoObuildHash = Just newBuildHash
    , ..
    }

setBuildKernelNames :: Environment Kernel.Type -> ProtoBuild a -> ProtoBuild a
setBuildKernelNames env ProtoBuild{..} =
  ProtoBuild
    { protoObuildKernelNames = env
    , ..
    }

setBuildKernelIRTypes :: Environment IRType -> ProtoBuild a -> ProtoBuild a
setBuildKernelIRTypes env ProtoBuild{..} =
  ProtoBuild
    { protoObuildKernelIRTypes = env
    , ..
    }

setBuildKernelConstructors :: Environment Int -> ProtoBuild a -> ProtoBuild a
setBuildKernelConstructors env ProtoBuild{..} =
  ProtoBuild
    { protoObuildKernelConstructors = env
    , ..
    }
