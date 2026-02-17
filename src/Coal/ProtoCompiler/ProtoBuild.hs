{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoBuild (
  ProtoBuild (..),
  protoOemptyBuild,
  overBuildNames,
  setBuildPath,
  --  setBuildFile,
  setBuildBitcode,
  setBuildHash,
  setBuildKernelNames,
  setBuildKernelIRTypes,
  setBuildKernelConstructors,
  insertBuildNameEntry,
  removeBuildNamePlaceholder,
  replaceBuildNameEntry,
  insertBuildExportedName,
  insertBuildDataConstructor,
  insertBuildTypeConstructor,
  insertBuildTrait,
  insertBuildInstance,
  insertBuildAlias,
  typeEnvironment,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build.Hash256 (Hash256 (..))
import Coal.Kernel.LLVM.IRType (IRType)
import qualified Coal.Kernel.Language as Kernel
import Coal.Language
import Coal.Language.Module.Path (Path (..))
import Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry
import Coal.ProtoLanguage.ProtoDefinition (ProtoDefinition (..))
import Control.Monad.State (execState, modify)
import Data.Binary
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Extras (Name, Set, forM_)
import GHC.Generics (Generic)

type InstanceMap a = Map IndexedType a

data ProtoBuild a = ProtoBuild
  { protoObuildPath :: Path
  , --  , protoObuildFile :: FilePath
    protoObuildNames :: Environment [ProtoNameEntry]
  , protoObuildExportedNames :: Set Name
  , protoObuildDataConstructors :: Environment (ProtoDataConstructorEntry a)
  , protoObuildTypeConstructors :: Environment (ProtoTypeConstructorEntry a)
  , protoObuildTraits :: Environment (ProtoTraitEntry a)
  , protoObuildInstances :: Environment (InstanceMap (ProtoInstanceEntry a))
  , protoObuildAliases :: Environment (ProtoAliasEntry a)
  , protoObuildBitcode :: Maybe ByteString
  , protoObuildHash :: Maybe Hash256
  , protoObuildKernelNames :: Environment Kernel.Type
  , protoObuildKernelIRTypes :: Environment IRType
  , protoObuildKernelConstructors :: Environment Int
  , protoObuildTypedDefinitions :: [ProtoDefinition a Kind IndexedType]
  }
  deriving (Show, Eq, Ord, Generic)

instance (Binary a) => Binary (ProtoBuild a)

protoOemptyBuild :: ProtoBuild a
protoOemptyBuild =
  ProtoBuild
    { protoObuildPath = Path []
    , --    , protoObuildFile = mempty
      protoObuildNames = mempty
    , protoObuildExportedNames = mempty
    , protoObuildDataConstructors = mempty
    , protoObuildTypeConstructors = mempty
    , protoObuildTraits = mempty
    , protoObuildInstances = mempty
    , protoObuildAliases = mempty
    , protoObuildBitcode = Nothing
    , protoObuildHash = Nothing
    , protoObuildKernelNames = mempty
    , protoObuildKernelIRTypes = mempty
    , protoObuildKernelConstructors = mempty
    , protoObuildTypedDefinitions = mempty
    }

setBuildPath :: Path -> ProtoBuild a -> ProtoBuild a
setBuildPath newBuildPath ProtoBuild{..} =
  ProtoBuild
    { protoObuildPath = newBuildPath
    , ..
    }

-- setBuildFile :: FilePath -> ProtoBuild a -> ProtoBuild a
-- setBuildFile newBuildFile ProtoBuild{..} =
--  ProtoBuild
--    { protoObuildFile = newBuildFile
--    , ..
--    }

overBuildNames :: (Environment [ProtoNameEntry] -> Environment [ProtoNameEntry]) -> ProtoBuild a -> ProtoBuild a
overBuildNames f ProtoBuild{..} =
  ProtoBuild
    { protoObuildNames = f protoObuildNames
    , ..
    }

insertBuildNameEntry :: ProtoNameEntry -> ProtoBuild a -> ProtoBuild a
insertBuildNameEntry entry =
  overBuildNames (Environment.insertWith (<>) name [entry])
 where
  name = protoOnameOf entry

removeBuildNamePlaceholder :: Name -> ProtoBuild a -> ProtoBuild a
removeBuildNamePlaceholder name =
  overBuildNames (Environment.adjust (filter (/= ProtoNPlaceholder name)) name)

replaceBuildNameEntry :: ProtoNameEntry -> ProtoBuild a -> ProtoBuild a
replaceBuildNameEntry entry =
  removeBuildNamePlaceholder name . insertBuildNameEntry entry
 where
  name = protoOnameOf entry

overBuildExportedNames :: (Set Name -> Set Name) -> ProtoBuild a -> ProtoBuild a
overBuildExportedNames f ProtoBuild{..} =
  ProtoBuild
    { protoObuildExportedNames = f protoObuildExportedNames
    , ..
    }

insertBuildExportedName :: Name -> ProtoBuild a -> ProtoBuild a
insertBuildExportedName name = overBuildExportedNames (Set.insert name)

overBuildDataConstructors :: (Environment (ProtoDataConstructorEntry a) -> Environment (ProtoDataConstructorEntry a)) -> ProtoBuild a -> ProtoBuild a
overBuildDataConstructors f ProtoBuild{..} =
  ProtoBuild
    { protoObuildDataConstructors = f protoObuildDataConstructors
    , ..
    }

insertBuildDataConstructor :: Name -> ProtoDataConstructorEntry a -> ProtoBuild a -> ProtoBuild a
insertBuildDataConstructor name = overBuildDataConstructors . Environment.insert name

overBuildTypeConstructors :: (Environment (ProtoTypeConstructorEntry a) -> Environment (ProtoTypeConstructorEntry a)) -> ProtoBuild a -> ProtoBuild a
overBuildTypeConstructors f ProtoBuild{..} =
  ProtoBuild
    { protoObuildTypeConstructors = f protoObuildTypeConstructors
    , ..
    }

insertBuildTypeConstructor :: Name -> ProtoTypeConstructorEntry a -> ProtoBuild a -> ProtoBuild a
insertBuildTypeConstructor name = overBuildTypeConstructors . Environment.insert name

overBuildTraits :: (Environment (ProtoTraitEntry a) -> Environment (ProtoTraitEntry a)) -> ProtoBuild a -> ProtoBuild a
overBuildTraits f ProtoBuild{..} =
  ProtoBuild
    { protoObuildTraits = f protoObuildTraits
    , ..
    }

insertBuildTrait :: Name -> ProtoTraitEntry a -> ProtoBuild a -> ProtoBuild a
insertBuildTrait name = overBuildTraits . Environment.insert name

overBuildInstances :: (Environment (InstanceMap (ProtoInstanceEntry a)) -> Environment (InstanceMap (ProtoInstanceEntry a))) -> ProtoBuild a -> ProtoBuild a
overBuildInstances f ProtoBuild{..} =
  ProtoBuild
    { protoObuildInstances = f protoObuildInstances
    , ..
    }

insertBuildInstance :: Name -> IndexedType -> ProtoInstanceEntry a -> ProtoBuild a -> ProtoBuild a
insertBuildInstance name t entry = overBuildInstances (Environment.alter (Just . f) name)
 where
  f =
    \case
      Nothing ->
        Map.singleton t entry
      Just m ->
        Map.insert t entry m

overBuildAliases :: (Environment (ProtoAliasEntry a) -> Environment (ProtoAliasEntry a)) -> ProtoBuild a -> ProtoBuild a
overBuildAliases f ProtoBuild{..} =
  ProtoBuild
    { protoObuildAliases = f protoObuildAliases
    , ..
    }

insertBuildAlias :: Name -> ProtoAliasEntry a -> ProtoBuild a -> ProtoBuild a
insertBuildAlias name = overBuildAliases . Environment.insert name

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

typeEnvironment :: ProtoBuild a -> Environment IndexedScheme
typeEnvironment ProtoBuild{..} =
  flip execState mempty $ do
    forM_ (concat $ Environment.elems protoObuildNames) $
      \case
        ProtoNName name s ->
          modify (Environment.insert name s)
        _ ->
          pure ()
