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
import Coal.Language (
  DataConstructor,
  IndexedScheme,
  IndexedType,
  Kind,
  Parameter,
  Scheme,
  Trait,
  Type,
  TypeIndex,
 )
import Coal.Language.Module.Export (Export (..))
import Coal.Language.Module.Import (Import (..))
import Data.Binary (Binary)
import Extras (Dictionary, Name, Set)
import GHC.Generics (Generic)

type IndexedConstructor = DataConstructor TypeIndex Kind IndexedType

data DataConstructorEntry a = DataConstructorEntry
  { dataConstructorEntryMetaData :: a
  , dataConstructorEntryName :: Name
  , dataConstructorEntryConstructor :: IndexedConstructor
  , dataConstructorEntryConstructorSet :: Set Name
  }
  deriving (Show, Eq, Ord, Read, Generic, Functor, Foldable, Traversable)

instance (Binary a) => Binary (DataConstructorEntry a)

data TypeConstructorEntry a = TypeConstructorEntry
  { typeConstructorEntryMetadata :: a
  , typeConstructorEntryName :: Name
  , typeConstructorEntryKind :: Kind
  , typeConstructorEntryDataConstructors :: [Name]
  }
  deriving (Show, Eq, Ord, Read, Generic, Functor, Foldable, Traversable)

instance (Binary a) => Binary (TypeConstructorEntry a)

data TraitEntry a = TraitEntry
  { traitEntryMetadata :: a
  , traitEntryName :: Name
  , traitEntryParameter :: Parameter Kind
  , traitEntryConstraints :: [Trait (Parameter Kind)]
  , traitEntryInterface :: Environment (Scheme Parameter Kind (Type Parameter Kind))
  }
  deriving (Show, Eq, Ord, Read, Generic, Functor, Foldable, Traversable)

instance (Binary a) => Binary (TraitEntry a)

data InstanceEntry a = InstanceEntry
  { instanceEntryMetadata :: a
  , instanceEntryType :: Type Parameter Kind
  , instanceEntryIndexedType :: IndexedType
  , instanceEntryTypeSchemes :: Dictionary IndexedScheme
  }
  deriving (Show, Eq, Ord, Read, Generic, Functor, Foldable, Traversable)

instance (Binary a) => Binary (InstanceEntry a)

data AliasEntry a = AliasEntry
  { aliasEntryMetadata :: a
  , aliasEntryName :: Name
  , aliasEntryParams :: [Parameter Kind]
  , aliasEntryType :: Type Parameter Kind
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
  nameOf :: a -> Name

instance HasName NameEntry where
  nameOf =
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
  nameOf =
    \case
      NameImport _ name ->
        name
      TypeImport _ name _ ->
        name

instance HasName (Export a) where
  nameOf =
    \case
      NameExport _ name ->
        name
      TypeExport _ name _ ->
        name

instance HasName (DataConstructorEntry a) where
  nameOf =
    dataConstructorEntryName
