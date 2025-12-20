{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Build (
  Hash256 (..),
  ModuleBuild (..),
  CotypeConstructorEntry (..),
  DataConstructorEntry (..),
  TypeConstructorEntry (..),
  CodataAccessorEntry (..),
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
  insertCodataAccessor,
  insertAlias,
  insertDataConstructor,
  insertCotypeConstructor,
  insertTypeConstructor,
  insertManyCodataAccessors,
  exportedCotypeConstructors,
  exportedCodataAccessors,
  exportedDataConstructors,
  exportedTypeConstructors,
  exportedTraits,
  exportedNames,
  exportedTypeNames,
  setExports,
  setTypeExports,
  setPath,
  setBitcode,
  setHash,
  insertHash,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build.NameEntry
import Coal.Language (IndexedType)
import Coal.Language.Module (Path (..))
import Crypto.Hash
import Data.Binary
import Data.Binary.Get (getByteString)
import Data.Binary.Put (putByteString)
import qualified Data.ByteArray as ByteArray
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Extras (Name, Set)
import GHC.Generics (Generic)

newtype Hash256 = Hash256 {unHash256 :: Digest SHA256}
  deriving (Eq, Ord, Show, Generic)

instance Binary Hash256 where
  put (Hash256 d) =
    putByteString (ByteArray.convert d)
  get = do
    bs <- getByteString 32
    case digestFromByteString bs of
      Just d ->
        pure (Hash256 d)
      Nothing ->
        fail "Invalid SHA256 digest"

data ModuleBuild a = ModuleBuild
  { modulePath :: Path
  , moduleDataConstructors :: Environment (DataConstructorEntry a)
  , moduleCodataAccessors :: Environment (CodataAccessorEntry a)
  , moduleTypeConstructors :: Environment (TypeConstructorEntry a)
  , moduleCotypeConstructors :: Environment (CotypeConstructorEntry a)
  , moduleTraits :: Environment (TraitEntry a)
  , moduleInstances :: Environment (Map IndexedType (InstanceEntry a))
  , moduleAliases :: Environment (AliasEntry a)
  , moduleNames :: [NameEntry]
  , moduleExports :: Set Name
  , moduleTypeExports :: Set Name
  , moduleBitcode :: Maybe ByteString
  , moduleHash :: Maybe Hash256
  }
  deriving (Show, Eq, Ord, Generic)

instance (Binary a) => Binary (ModuleBuild a)

memberOf :: (HasName a) => Set Name -> a -> Bool
memberOf s info = nameOf info `Set.member` s

exportedNames :: ModuleBuild a -> [NameEntry]
exportedNames ModuleBuild{..} = filter (memberOf moduleExports) moduleNames

exportedTypeNames :: ModuleBuild a -> [NameEntry]
exportedTypeNames ModuleBuild{..} = filter (memberOf moduleTypeExports) moduleNames

exportedTypeConstructors :: ModuleBuild a -> Environment (TypeConstructorEntry a)
exportedTypeConstructors ModuleBuild{..} = Environment.filter (memberOf moduleTypeExports) moduleTypeConstructors

exportedCotypeConstructors :: ModuleBuild a -> Environment (CotypeConstructorEntry a)
exportedCotypeConstructors ModuleBuild{..} = Environment.filter (memberOf moduleTypeExports) moduleCotypeConstructors

exportedDataConstructors :: ModuleBuild a -> Environment (DataConstructorEntry a)
exportedDataConstructors ModuleBuild{..} = Environment.filter (memberOf moduleExports) moduleDataConstructors

exportedCodataAccessors :: ModuleBuild a -> Environment (CodataAccessorEntry a)
exportedCodataAccessors ModuleBuild{..} = Environment.filter (memberOf moduleExports) moduleCodataAccessors

exportedTraits :: ModuleBuild a -> [TraitEntry a]
exportedTraits ModuleBuild{..} = snd <$> filter (memberOf moduleTypeExports) (Environment.toList moduleTraits)

emptyModuleBuild :: ModuleBuild a
emptyModuleBuild =
  ModuleBuild
    { modulePath = Path []
    , moduleDataConstructors = mempty
    , moduleCodataAccessors = mempty
    , moduleTypeConstructors = mempty
    , moduleCotypeConstructors = mempty
    , moduleTraits = mempty
    , moduleInstances = mempty
    , moduleAliases = mempty
    , moduleNames = mempty
    , moduleExports = mempty
    , moduleTypeExports = mempty
    , moduleBitcode = Nothing
    , moduleHash = Nothing
    }

insertDataConstructor :: Name -> DataConstructorEntry a -> ModuleBuild a -> ModuleBuild a
insertDataConstructor name info ModuleBuild{..} =
  ModuleBuild
    { moduleDataConstructors =
        Environment.insert name info moduleDataConstructors
    , ..
    }

insertCodataAccessor :: Name -> CodataAccessorEntry a -> ModuleBuild a -> ModuleBuild a
insertCodataAccessor name info ModuleBuild{..} =
  ModuleBuild
    { moduleCodataAccessors =
        Environment.insert name info moduleCodataAccessors
    , ..
    }

insertManyCodataAccessors :: [CodataAccessorEntry a] -> ModuleBuild a -> ModuleBuild a
insertManyCodataAccessors infos ModuleBuild{..} =
  ModuleBuild
    { moduleCodataAccessors =
        Environment.insertMultiple
          [(name, info) | info@(CodataAccessorEntry _ name _) <- infos]
          moduleCodataAccessors
    , ..
    }

insertTypeConstructor :: Name -> TypeConstructorEntry a -> ModuleBuild a -> ModuleBuild a
insertTypeConstructor name info ModuleBuild{..} =
  ModuleBuild
    { moduleTypeConstructors =
        Environment.insert name info moduleTypeConstructors
    , ..
    }

insertCotypeConstructor :: Name -> CotypeConstructorEntry a -> ModuleBuild a -> ModuleBuild a
insertCotypeConstructor name info ModuleBuild{..} =
  ModuleBuild
    { moduleCotypeConstructors =
        Environment.insert name info moduleCotypeConstructors
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
    { moduleExports = Set.insert name moduleExports
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
    { moduleExports = Set.fromList names
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
    { modulePath = path
    , ..
    }

setBitcode :: ByteString -> ModuleBuild a -> ModuleBuild a
setBitcode code ModuleBuild{..} =
  ModuleBuild
    { moduleBitcode = Just code
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
