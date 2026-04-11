-- +
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Build.NameEntry (
  DataConstructorEntry (..),
  TypeConstructorEntry (..),
  TraitEntry (..),
  InstanceEntry (..),
  AliasEntry (..),
  NameEntry (..),
  HasName (..),
) where

import Coal.Common.Environment (Environment (..))
import Coal.Language
import Coal.Language.Module.Export (Export (..))
import Coal.Language.Module.Import (Import (..))
import Data.Binary (Binary)
import Extras (Dictionary, Name, Set)
import GHC.Generics (Generic)

type IndexedConstructor = DataConstructor TypeIndex Kind IndexedType

data DataConstructorEntry a = DataConstructorEntry
  { protoOdataConstructorEntryMetaData :: a
  , protoOdataConstructorEntryName :: Name
  , protoOdataConstructorEntryConstructor :: IndexedConstructor
  , protoOdataConstructorEntryConstructorSet :: Set Name
  }
  deriving (Show, Eq, Ord, Read, Generic, Functor, Foldable, Traversable)

instance (Binary a) => Binary (DataConstructorEntry a)

data TypeConstructorEntry a = TypeConstructorEntry
  { protoOtypeConstructorEntryMetadata :: a
  , protoOtypeConstructorEntryName :: Name
  , protoOtypeConstructorEntryKind :: Kind
  , protoOtypeConstructorEntryDataConstructors :: [Name]
  }
  deriving (Show, Eq, Ord, Read, Generic, Functor, Foldable, Traversable)

instance (Binary a) => Binary (TypeConstructorEntry a)

data TraitEntry a = TraitEntry
  { protoOtraitEntryMetadata :: a
  , protoOtraitEntryName :: Name
  , protoOtraitEntryParameter :: Parameter Kind
  , protoOtraitEntryConstraints :: [Trait (Parameter Kind)]
  , protoOtraitEntryInterface :: Environment (Scheme Parameter Kind (Type Parameter Kind))
  }
  deriving (Show, Eq, Ord, Read, Generic, Functor, Foldable, Traversable)

instance (Binary a) => Binary (TraitEntry a)

data InstanceEntry a = InstanceEntry
  { protoOinstanceEntryMetadata :: a
  , protoOinstanceEntryType :: Type Parameter Kind
  , protoOinstanceEntryIndexedType :: IndexedType
  , protoOinstanceEntryTypeSchemes :: Dictionary IndexedScheme
  }
  deriving (Show, Eq, Ord, Read, Generic, Functor, Foldable, Traversable)

instance (Binary a) => Binary (InstanceEntry a)

data AliasEntry a = AliasEntry
  { protoOaliasEntryMetadata :: a
  , protoOaliasEntryName :: Name
  , protoOaliasEntryParams :: [Parameter Kind]
  , protoOaliasEntryType :: Type Parameter Kind
  }
  deriving (Show, Eq, Ord, Read, Generic, Functor, Foldable, Traversable)

instance (Binary a) => Binary (AliasEntry a)

data NameEntry
  = NName Name IndexedScheme
  | NType Name Kind
  | NTrait Name
  | NTypeAlias Name
  | NPlaceholder Name
  deriving (Show, Eq, Ord, Read, Generic)

instance Binary NameEntry

class HasName a where
  protoOnameOf :: a -> Name

instance HasName NameEntry where
  protoOnameOf =
    \case
      NName name _ ->
        name
      NType name _ ->
        name
      NTrait name ->
        name
      NTypeAlias name ->
        name
      NPlaceholder name ->
        name

instance HasName (Import a) where
  protoOnameOf =
    \case
      NameImport _ name ->
        name
      TypeImport _ name _ ->
        name

instance HasName (Export a) where
  protoOnameOf =
    \case
      NameExport _ name ->
        name
      TypeExport _ name _ ->
        name

instance HasName (DataConstructorEntry a) where
  protoOnameOf =
    protoOdataConstructorEntryName
