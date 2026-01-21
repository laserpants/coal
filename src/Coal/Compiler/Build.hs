{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Build (
  Hash256 (..),
  ModuleBuild (..),
  DataConstructorEntry (..),
  TypeConstructorEntry (..),
  TraitEntry (..),
  InstanceEntry (..),
  AliasEntry (..),
  NameEntry (..),
  HasName (..),
  emptyModuleBuild,
  addName,
  addExport,
  addTypeExport,
  insertInstance,
  insertTrait,
  insertAlias,
  insertDataConstructor,
  insertTypeConstructor,
  exportedDataConstructors,
  exportedTypeConstructors,
  exportedTraits,
  exportedNames,
  exportedTypeNames,
  setExports,
  setTypeExports,
  setPath,
  setBitcode,
  setDependencies,
  setHash,
  insertHash,
  setQualifiedNames,
  setKernelNames,
  setKernelIRTypes,
  setKernelConstructors,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build.Hash256 (Hash256 (..))
import Coal.Compiler.Build.NameEntry
import Coal.Kernel.LLVM.IRType (IRType)
import qualified Coal.Kernel.Language as Kernel
import Coal.Language (IndexedType)
import Coal.Language.Module (Path (..))
import Crypto.Hash
import Data.Binary
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Extras (Name, Set)
import GHC.Generics (Generic)

data ModuleBuild a = ModuleBuild
  { moduleBuildPath :: Path
  , moduleFilePath :: Text
  , moduleDataConstructors :: Environment (DataConstructorEntry a)
  , moduleTypeConstructors :: Environment (TypeConstructorEntry a)
  , moduleTraits :: Environment (TraitEntry a)
  , moduleInstances :: Environment (Map IndexedType (InstanceEntry a))
  , moduleAliases :: Environment (AliasEntry a)
  , moduleNames :: [NameEntry]
  , moduleDependencies :: [Path]
  , moduleQualifiedNames :: Environment Name
  , moduleExportedNames :: Set Name
  , moduleTypeExports :: Set Name
  , moduleBitcode :: Maybe ByteString
  , moduleHash :: Maybe Hash256
  , moduleKernelNames :: Environment Kernel.Type
  , moduleKernelIRTypes :: Environment IRType
  , moduleKernelConstructors :: Environment Int
  }
  deriving (Show, Eq, Ord, Generic)

instance (Binary a) => Binary (ModuleBuild a)

memberOf :: (HasName a) => Set Name -> a -> Bool
memberOf s info = nameOf info `Set.member` s

exportedNames :: ModuleBuild a -> [NameEntry]
exportedNames ModuleBuild{..} = filter (memberOf moduleExportedNames) moduleNames

exportedTypeNames :: ModuleBuild a -> [NameEntry]
exportedTypeNames ModuleBuild{..} = filter (memberOf moduleTypeExports) moduleNames

exportedTypeConstructors :: ModuleBuild a -> Environment (TypeConstructorEntry a)
exportedTypeConstructors ModuleBuild{..} = Environment.filter (memberOf moduleTypeExports) moduleTypeConstructors

exportedDataConstructors :: ModuleBuild a -> Environment (DataConstructorEntry a)
exportedDataConstructors ModuleBuild{..} = Environment.filter (memberOf moduleExportedNames) moduleDataConstructors

exportedTraits :: ModuleBuild a -> [TraitEntry a]
exportedTraits ModuleBuild{..} = snd <$> filter (memberOf moduleTypeExports) (Environment.toList moduleTraits)

emptyModuleBuild :: ModuleBuild a
emptyModuleBuild =
  ModuleBuild
    { moduleBuildPath = Path []
    , moduleFilePath = mempty
    , moduleDataConstructors = mempty
    , moduleTypeConstructors = mempty
    , moduleTraits = mempty
    , moduleInstances = mempty
    , moduleAliases = mempty
    , moduleNames = mempty
    , moduleDependencies = mempty
    , moduleQualifiedNames = mempty
    , moduleExportedNames = mempty
    , moduleTypeExports = mempty
    , moduleBitcode = Nothing
    , moduleHash = Nothing
    , moduleKernelNames = mempty
    , moduleKernelIRTypes = mempty
    , moduleKernelConstructors = mempty
    }

insertDataConstructor :: Name -> DataConstructorEntry a -> ModuleBuild a -> ModuleBuild a
insertDataConstructor name info ModuleBuild{..} =
  ModuleBuild
    { moduleDataConstructors =
        Environment.insert name info moduleDataConstructors
    , ..
    }

insertTypeConstructor :: Name -> TypeConstructorEntry a -> ModuleBuild a -> ModuleBuild a
insertTypeConstructor name info ModuleBuild{..} =
  ModuleBuild
    { moduleTypeConstructors =
        Environment.insert name info moduleTypeConstructors
    , ..
    }

insertTrait :: Name -> TraitEntry a -> ModuleBuild a -> ModuleBuild a
insertTrait name info ModuleBuild{..} =
  ModuleBuild
    { moduleTraits =
        Environment.insert name info moduleTraits
    , ..
    }

insertInstance :: Name -> IndexedType -> InstanceEntry a -> ModuleBuild a -> ModuleBuild a
insertInstance name it info ModuleBuild{..} =
  ModuleBuild
    { moduleInstances =
        Environment.insert name (Map.insert it info entries) moduleInstances
    , ..
    }
 where
  entries = fromMaybe mempty (Environment.lookup name moduleInstances)

insertAlias :: Name -> AliasEntry a -> ModuleBuild a -> ModuleBuild a
insertAlias name info ModuleBuild{..} =
  ModuleBuild
    { moduleAliases = Environment.insert name info moduleAliases
    , ..
    }

addName :: NameEntry -> ModuleBuild a -> ModuleBuild a
addName info ModuleBuild{..} =
  ModuleBuild
    { moduleNames = info : moduleNames
    , ..
    }

addExport :: Name -> ModuleBuild a -> ModuleBuild a
addExport name ModuleBuild{..} =
  ModuleBuild
    { moduleExportedNames = Set.insert name moduleExportedNames
    , ..
    }

addTypeExport :: Name -> ModuleBuild a -> ModuleBuild a
addTypeExport name ModuleBuild{..} =
  ModuleBuild
    { moduleTypeExports = Set.insert name moduleTypeExports
    , ..
    }

setExports :: [Name] -> ModuleBuild a -> ModuleBuild a
setExports names ModuleBuild{..} =
  ModuleBuild
    { moduleExportedNames = Set.fromList names
    , ..
    }

setTypeExports :: [Name] -> ModuleBuild a -> ModuleBuild a
setTypeExports names ModuleBuild{..} =
  ModuleBuild
    { moduleTypeExports = Set.fromList names
    , ..
    }

setPath :: Path -> ModuleBuild a -> ModuleBuild a
setPath path ModuleBuild{..} =
  ModuleBuild
    { moduleBuildPath = path
    , ..
    }

setBitcode :: ByteString -> ModuleBuild a -> ModuleBuild a
setBitcode code ModuleBuild{..} =
  ModuleBuild
    { moduleBitcode = Just code
    , ..
    }

setDependencies :: [Path] -> ModuleBuild a -> ModuleBuild a
setDependencies paths ModuleBuild{..} =
  ModuleBuild
    { moduleDependencies = paths
    , ..
    }

setHash :: Hash256 -> ModuleBuild a -> ModuleBuild a
setHash hash256 ModuleBuild{..} =
  ModuleBuild
    { moduleHash = Just hash256
    , ..
    }

insertHash :: Text -> ModuleBuild a -> ModuleBuild a
insertHash source = setHash (Hash256 (hash (Text.encodeUtf8 source)))

setQualifiedNames :: Environment Name -> ModuleBuild a -> ModuleBuild a
setQualifiedNames names ModuleBuild{..} =
  ModuleBuild
    { moduleQualifiedNames = names
    , ..
    }

setKernelNames :: Environment Kernel.Type -> ModuleBuild a -> ModuleBuild a
setKernelNames env ModuleBuild{..} =
  ModuleBuild
    { moduleKernelNames = env
    , ..
    }

setKernelIRTypes :: Environment IRType -> ModuleBuild a -> ModuleBuild a
setKernelIRTypes env ModuleBuild{..} =
  ModuleBuild
    { moduleKernelIRTypes = env
    , ..
    }

setKernelConstructors :: Environment Int -> ModuleBuild a -> ModuleBuild a
setKernelConstructors env ModuleBuild{..} =
  ModuleBuild
    { moduleKernelConstructors = env
    , ..
    }
