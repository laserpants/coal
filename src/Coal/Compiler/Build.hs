{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Build (
  Build (..),
  InstanceMap,
  emptyBuild,
  overBuildNames,
  setBuildPath,
  --  setBuildFile,
  setBuildBitcode,
  setBuildHash,
  setBuildKernelNames,
  setBuildKernelIRTypes,
  setBuildKernelConstructors,
  setQualifiedNames,
  setBuildSource,
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
  overBuildDataConstructors,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build.Hash256 (Hash256 (..))
import Coal.Compiler.Build.NameEntry
import Coal.Kernel.LLVM.IRType (IRType)
import qualified Coal.Kernel.Language as Kernel
import Coal.Language
import Coal.Language.Module.Path (Path (..))
import Control.Monad.State (execState, modify)
import Data.Binary
import Data.ByteString (ByteString)
import Data.List (nubBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Extras (Name, Set, forM_)
import GHC.Generics (Generic)

type InstanceMap a = Map IndexedType a

data Build a = Build
  { buildPath :: Path
  , --  , buildFile :: FilePath
    buildNames :: Environment [NameEntry]
  , buildExportedNames :: Set Name
  , buildDataConstructors :: Environment (DataConstructorEntry a)
  , -- folds?
    buildTypeConstructors :: Environment (TypeConstructorEntry a)
  , buildTraits :: Environment (TraitEntry a)
  , buildInstances :: Environment (InstanceMap (InstanceEntry a))
  , buildAliases :: Environment (AliasEntry a)
  , buildDependencies :: [Path]
  , buildQualifiedNames :: Environment Name
  , buildBitcode :: Maybe ByteString
  , buildHash :: Maybe Hash256
  , buildSource :: Text
  , buildKernelNames :: Environment Kernel.Type
  , buildKernelIRTypes :: Environment IRType
  , buildKernelConstructors :: Environment Int
  --  , buildTypedDefinitions :: [Definition a Kind IndexedType]
  }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

instance (Binary a) => Binary (Build a)

emptyBuild :: Build a
emptyBuild =
  Build
    { buildPath = Path []
    , --    , buildFile = mempty
      buildNames = mempty
    , buildExportedNames = mempty
    , buildDataConstructors = mempty
    , buildTypeConstructors = mempty
    , buildTraits = mempty
    , buildInstances = mempty
    , buildAliases = mempty
    , buildDependencies = mempty
    , buildQualifiedNames = mempty
    , buildBitcode = Nothing
    , buildHash = Nothing
    , buildSource = mempty
    , buildKernelNames = mempty
    , buildKernelIRTypes = mempty
    , buildKernelConstructors = mempty
    --   , buildTypedDefinitions = mempty
    }

setBuildPath :: Path -> Build a -> Build a
setBuildPath newBuildPath Build{..} =
  Build
    { buildPath = newBuildPath
    , ..
    }

-- setBuildFile :: FilePath -> Build a -> Build a
-- setBuildFile newBuildFile Build{..} =
--  Build
--    { buildFile = newBuildFile
--    , ..
--    }

overBuildNames :: (Environment [NameEntry] -> Environment [NameEntry]) -> Build a -> Build a
overBuildNames f Build{..} =
  Build
    { buildNames = f buildNames
    , ..
    }

nameEntryEquality :: NameEntry -> NameEntry -> Bool
nameEntryEquality a b =
  case (a, b) of
    (NName n1 _, NName n2 _)
      | n1 == n2 -> True
    (NType n1 _, NType n2 _)
      | n1 == n2 -> True
    (NTrait n1, NTrait n2)
      | n1 == n2 -> True
    (NTypeAlias n1, NTypeAlias n2)
      | n1 == n2 -> True
    (NPlaceholder n1, NPlaceholder n2)
      | n1 == n2 -> True
    (_, _) ->
      False

insertBuildNameEntry :: NameEntry -> Build a -> Build a
insertBuildNameEntry entry =
  overBuildNames (Environment.adjust (nubBy nameEntryEquality) name . Environment.insertWith (<>) name [entry])
 where
  name = nameOf entry

removeBuildNamePlaceholder :: Name -> Build a -> Build a
removeBuildNamePlaceholder name =
  overBuildNames (Environment.adjust (filter (/= NPlaceholder name)) name)

replaceBuildNameEntry :: NameEntry -> Build a -> Build a
replaceBuildNameEntry entry =
  removeBuildNamePlaceholder name . insertBuildNameEntry entry
 where
  name = nameOf entry

overBuildExportedNames :: (Set Name -> Set Name) -> Build a -> Build a
overBuildExportedNames f Build{..} =
  Build
    { buildExportedNames = f buildExportedNames
    , ..
    }

insertBuildExportedName :: Name -> Build a -> Build a
insertBuildExportedName name = overBuildExportedNames (Set.insert name)

overBuildDataConstructors :: (Environment (DataConstructorEntry a) -> Environment (DataConstructorEntry a)) -> Build a -> Build a
overBuildDataConstructors f Build{..} =
  Build
    { buildDataConstructors = f buildDataConstructors
    , ..
    }

insertBuildDataConstructor :: Name -> DataConstructorEntry a -> Build a -> Build a
insertBuildDataConstructor name = overBuildDataConstructors . Environment.insert name

overBuildTypeConstructors :: (Environment (TypeConstructorEntry a) -> Environment (TypeConstructorEntry a)) -> Build a -> Build a
overBuildTypeConstructors f Build{..} =
  Build
    { buildTypeConstructors = f buildTypeConstructors
    , ..
    }

insertBuildTypeConstructor :: Name -> TypeConstructorEntry a -> Build a -> Build a
insertBuildTypeConstructor name = overBuildTypeConstructors . Environment.insert name

overBuildTraits :: (Environment (TraitEntry a) -> Environment (TraitEntry a)) -> Build a -> Build a
overBuildTraits f Build{..} =
  Build
    { buildTraits = f buildTraits
    , ..
    }

insertBuildTrait :: Name -> TraitEntry a -> Build a -> Build a
insertBuildTrait name = overBuildTraits . Environment.insert name

overBuildInstances :: (Environment (InstanceMap (InstanceEntry a)) -> Environment (InstanceMap (InstanceEntry a))) -> Build a -> Build a
overBuildInstances f Build{..} =
  Build
    { buildInstances = f buildInstances
    , ..
    }

insertBuildInstance :: Name -> IndexedType -> InstanceEntry a -> Build a -> Build a
insertBuildInstance name t entry = overBuildInstances (Environment.alter (Just . f) name)
 where
  f =
    \case
      Nothing ->
        Map.singleton t entry
      Just m ->
        Map.insert t entry m

overBuildAliases :: (Environment (AliasEntry a) -> Environment (AliasEntry a)) -> Build a -> Build a
overBuildAliases f Build{..} =
  Build
    { buildAliases = f buildAliases
    , ..
    }

-- overBuildQualifiedNames :: (Environment Name -> Environment Name) -> Build a -> Build a
-- overBuildQualifiedNames f Build{..} =
--  Build
--    { buildQualifiedNames = f buildQualifiedNames
--    , ..
--    }

insertBuildAlias :: Name -> AliasEntry a -> Build a -> Build a
insertBuildAlias name = overBuildAliases . Environment.insert name

setBuildBitcode :: ByteString -> Build a -> Build a
setBuildBitcode newBuildBitcode Build{..} =
  Build
    { buildBitcode = Just newBuildBitcode
    , ..
    }

setBuildHash :: Hash256 -> Build a -> Build a
setBuildHash newBuildHash Build{..} =
  Build
    { buildHash = Just newBuildHash
    , ..
    }

setBuildKernelNames :: Environment Kernel.Type -> Build a -> Build a
setBuildKernelNames env Build{..} =
  Build
    { buildKernelNames = env
    , ..
    }

setBuildKernelIRTypes :: Environment IRType -> Build a -> Build a
setBuildKernelIRTypes env Build{..} =
  Build
    { buildKernelIRTypes = env
    , ..
    }

setBuildKernelConstructors :: Environment Int -> Build a -> Build a
setBuildKernelConstructors env Build{..} =
  Build
    { buildKernelConstructors = env
    , ..
    }

setQualifiedNames :: Environment Name -> Build a -> Build a
setQualifiedNames names Build{..} =
  Build
    { buildQualifiedNames = names
    , ..
    }

setBuildSource :: Text -> Build a -> Build a
setBuildSource source Build{..} =
  Build
    { buildSource = source
    , ..
    }

-- TODO: rename
typeEnvironment :: Build a -> Environment IndexedScheme
typeEnvironment Build{..} =
  flip execState mempty $ do
    forM_ (concat $ Environment.elems buildNames) $
      \case
        NName name s ->
          modify (Environment.insert name s)
        _ ->
          pure ()
