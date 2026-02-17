{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry (
  ProtoDataConstructorEntry (..),
  ProtoTypeConstructorEntry (..),
  ProtoTraitEntry (..),
  ProtoInstanceEntry (..),
  ProtoAliasEntry (..),
  ProtoNameEntry (..),
  ProtoHasName (..),
) where

import Coal.Common.Environment (Environment (..))
import Coal.Language
import Coal.Language.Module.Export (Export (..))
import Coal.Language.Module.Import (Import (..))
import Data.Binary (Binary)
import Extras (Dictionary, Name, Set)
import GHC.Generics (Generic)

type IndexedConstructor = DataConstructor TypeIndex Kind IndexedType

data ProtoDataConstructorEntry a = ProtoDataConstructorEntry
  { protoOdataConstructorEntryMetaData :: a
  , protoOdataConstructorEntryName :: Name
  , protoOdataConstructorEntryConstructor :: IndexedConstructor
  , protoOdataConstructorEntryConstructorSet :: Set Name
  }
  deriving (Show, Eq, Ord, Read, Generic)

instance (Binary a) => Binary (ProtoDataConstructorEntry a)

data ProtoTypeConstructorEntry a = ProtoTypeConstructorEntry
  { protoOtypeConstructorEntryMetadata :: a
  , protoOtypeConstructorEntryName :: Name
  , protoOtypeConstructorEntryKind :: Kind
  , protoOtypeConstructorEntryDataConstructors :: [Name]
  }
  deriving (Show, Eq, Ord, Read, Generic)

instance (Binary a) => Binary (ProtoTypeConstructorEntry a)

data ProtoTraitEntry a = ProtoTraitEntry
  { protoOtraitEntryMetadata :: a
  , protoOtraitEntryName :: Name
  , protoOtraitEntryParameter :: Parameter Kind
  , protoOtraitEntryConstraints :: [Trait (Parameter Kind)]
  , protoOtraitEntryInterface :: Environment (Scheme Parameter Kind (Type Parameter Kind))
  }
  deriving (Show, Eq, Ord, Read, Generic)

instance (Binary a) => Binary (ProtoTraitEntry a)

data ProtoInstanceEntry a = ProtoInstanceEntry
  { protoOinstanceEntryMetadata :: a
  , protoOinstanceEntryType :: Type Parameter Kind
  , protoOinstanceEntryIndexedType :: IndexedType
  , protoOinstanceEntryTypeSchemes :: Dictionary IndexedScheme
  }
  deriving (Show, Eq, Ord, Read, Generic)

instance (Binary a) => Binary (ProtoInstanceEntry a)

data ProtoAliasEntry a = ProtoAliasEntry
  { protoOaliasEntryMetadata :: a
  , protoOaliasEntryName :: Name
  , protoOaliasEntryParams :: [Name]
  , protoOaliasEntryType :: Type Parameter Kind
  }
  deriving (Show, Eq, Ord, Read, Generic)

instance (Binary a) => Binary (ProtoAliasEntry a)

data ProtoNameEntry
  = ProtoNName Name IndexedScheme
  | ProtoNType Name Kind
  | ProtoNTrait Name
  | ProtoNTypeAlias Name
  | ProtoNPlaceholder Name
  deriving (Show, Eq, Ord, Read, Generic)

instance Binary ProtoNameEntry

class ProtoHasName a where
  protoOnameOf :: a -> Name

instance ProtoHasName ProtoNameEntry where
  protoOnameOf =
    \case
      ProtoNName name _ ->
        name
      ProtoNType name _ ->
        name
      ProtoNTrait name ->
        name
      ProtoNTypeAlias name ->
        name
      ProtoNPlaceholder name ->
        name

instance ProtoHasName (Import a) where
  protoOnameOf =
    \case
      NameImport _ name ->
        name
      TypeImport _ name _ ->
        name

instance ProtoHasName (Export a) where
  protoOnameOf =
    \case
      NameExport _ name ->
        name
      TypeExport _ name _ ->
        name

instance ProtoHasName (ProtoDataConstructorEntry a) where
  protoOnameOf =
    protoOdataConstructorEntryName
