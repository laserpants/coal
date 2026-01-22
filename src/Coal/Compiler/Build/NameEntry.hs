{-# LANGUAGE DeriveGeneric #-}
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
import Data.Binary (Binary)
import Extras (Dictionary, Name, Set)
import GHC.Generics (Generic)

type IndexedConstructor = DataConstructor TypeIndex Kind IndexedType

data DataConstructorEntry a = DataConstructorEntry
  { dataConstructorEntryMetaData :: a
  , dataConstructorEntryName :: Name
  , dataConstructorEntryConstructor :: IndexedConstructor
  , dataConstructorEntryNameSet :: Set Name
  }
  deriving (Show, Eq, Ord, Read, Generic)

instance (Binary a) => Binary (DataConstructorEntry a)

data TypeConstructorEntry a = TypeConstructorEntry
  { typeConstructorEntryMetadata :: a
  , typeConstructorEntryName :: Name
  , typeConstructorEntryKind :: Kind
  , typeConstructorEntryDataConstructors :: [Name]
  }
  deriving (Show, Eq, Ord, Read, Generic)

instance (Binary a) => Binary (TypeConstructorEntry a)

data TraitEntry a = TraitEntry
  { traitEntryMetadata :: a
  , traitEntryName :: Name
  , traitEntryParameter :: Parameter Kind
  , traitEntryRequiredInstances :: [Trait (Parameter Kind)]
  , -- TODO: methods?
    traitEntryEntries :: Environment (Scheme Parameter Kind (Type Parameter Kind))
  }
  deriving (Show, Eq, Ord, Read, Generic)

instance (Binary a) => Binary (TraitEntry a)

data InstanceEntry a = InstanceEntry
  { instanceEntryMetadata :: a
  , instanceEntryType :: ParameterizedType
  , instanceEntryIndexedType :: IndexedType
  , -- TODO: methods?
    instanceEntryEntries :: Dictionary IndexedScheme
  }
  deriving (Show, Eq, Ord, Read, Generic)

instance (Binary a) => Binary (InstanceEntry a)

data AliasEntry a = AliasEntry
  { aliasEntryMetadata :: a
  , aliasEntryName :: Name
  , aliasEntryParams :: [Name]
  , aliasEntryType :: ParameterizedType
  }
  deriving (Show, Eq, Ord, Read, Generic)

instance (Binary a) => Binary (AliasEntry a)

data NameEntry
  = NFunction Name IndexedScheme
  | NConstant Name IndexedScheme
  | NFold Name IndexedScheme
  | NDataConstructor Name IndexedScheme
  | NType Name Kind
  | NTrait Name
  | NAlias Name
  | NFunctionPlaceholder Name
  | NConstantPlaceholder Name
  | NFoldPlaceholder Name
  deriving (Show, Eq, Ord, Read, Generic)

instance Binary NameEntry

class HasName a where
  nameOf :: a -> Name

instance HasName NameEntry where
  nameOf =
    \case
      NFunction name _ ->
        name
      NConstant name _ ->
        name
      NFold name _ ->
        name
      NDataConstructor name _ ->
        name
      NType name _ ->
        name
      NTrait name ->
        name
      NAlias name ->
        name
      NFunctionPlaceholder name ->
        name
      NConstantPlaceholder name ->
        name
      NFoldPlaceholder name ->
        name

instance HasName (DataConstructorEntry a) where
  nameOf = dataConstructorEntryName

instance HasName (TypeConstructorEntry a) where
  nameOf = typeConstructorEntryName

instance HasName (TraitEntry a) where
  nameOf = traitEntryName

instance HasName (Name, a) where
  nameOf = fst
