{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Build.NameEntry (
  DataConstructorEntry (..),
  CodataAccessorEntry (..),
  TypeConstructorEntry (..),
  CotypeConstructorEntry (..),
  TraitEntry (..),
  InstanceEntry (..),
  AliasEntry (..),
  NameEntry (..),
  HasName (..),
) where

import Coal.Common.Environment (Environment (..))
import Coal.Language
import Extras (Dictionary, Name, Set)

type IndexedConstructor = DataConstructor TypeIndex Kind IndexedType

data DataConstructorEntry a = DataConstructorEntry
  { dataConstructorEntryMetaData :: a
  , dataConstructorEntryName :: Name
  , dataConstructorEntryConstructor :: IndexedConstructor
  , dataConstructorEntryNameSet :: Set Name
  }
  deriving (Show, Eq, Ord, Read)

type IndexedCodataAccessor = CodataAccessor TypeIndex Kind IndexedType

data CodataAccessorEntry a = CodataAccessorEntry
  { codataAccessorEntryMetadata :: a
  , codataAccessorEntryName :: Name
  , codataAccessorEntryAccessor :: IndexedCodataAccessor
  }
  deriving (Show, Eq, Ord, Read)

data TypeConstructorEntry a = TypeConstructorEntry
  { typeConstructorEntryMetadata :: a
  , typeConstructorEntryName :: Name
  , typeConstructorEntryKind :: Kind
  , typeConstructorEntryDataConstructors :: [Name]
  }
  deriving (Show, Eq, Ord, Read)

data CotypeConstructorEntry a = CotypeConstructorEntry
  { cotypeConstructorEntryMetadata :: a
  , cotypeConstructorEntryName :: Name
  , cotypeConstructorEntryKind :: Kind
  , cotypeConstructorEntryDataAccessors :: [Name]
  }
  deriving (Show, Eq, Ord, Read)

data TraitEntry a = TraitEntry
  { traitEntryMetadata :: a
  , traitEntryName :: Name
  , traitEntryParameter :: Parameter Kind
  , traitEntryEntries :: Environment (Scheme Parameter () ParameterizedType)
  }
  deriving (Show, Eq, Ord, Read)

data InstanceEntry a = InstanceEntry
  { instanceEntryMetadata :: a
  , instanceEntryType :: ParameterizedType
  , instanceEntryIndexedType :: IndexedType
  , instanceEntryEntries :: Dictionary IndexedScheme
  }
  deriving (Show, Eq, Ord, Read)

data AliasEntry a = AliasEntry
  { aliasEntryMetadata :: a
  , aliasEntryName :: Name
  , aliasEntryParams :: [Name]
  , aliasEntryType :: ParameterizedType
  }
  deriving (Show, Eq, Ord, Read)

data NameEntry
  = IFunction Name IndexedScheme
  | IConstant Name IndexedScheme
  | IFold Name IndexedScheme
  | IUnfold Name IndexedScheme
  | IDataConstructor Name IndexedScheme
  | ICodataAccessor Name IndexedScheme
  | IType Name Kind
  | ICotype Name Kind
  | ITrait Name
  | IAlias Name
  | IFunctionPlaceholder Name
  | IConstantPlaceholder Name
  | IFoldPlaceholder Name
  | IUnfoldPlaceholder Name
  deriving (Show, Eq, Ord, Read)

class HasName a where
  nameOf :: a -> Name

instance HasName NameEntry where
  nameOf =
    \case
      IFunction name _ ->
        name
      IConstant name _ ->
        name
      IFold name _ ->
        name
      IUnfold name _ ->
        name
      IDataConstructor name _ ->
        name
      ICodataAccessor name _ ->
        name
      IType name _ ->
        name
      ICotype name _ ->
        name
      ITrait name ->
        name
      IAlias name ->
        name
      IFunctionPlaceholder name ->
        name
      IConstantPlaceholder name ->
        name
      IFoldPlaceholder name ->
        name
      IUnfoldPlaceholder name ->
        name

instance HasName (DataConstructorEntry a) where
  nameOf = dataConstructorEntryName

instance HasName (CodataAccessorEntry a) where
  nameOf = codataAccessorEntryName

instance HasName (TypeConstructorEntry a) where
  nameOf = typeConstructorEntryName

instance HasName (CotypeConstructorEntry a) where
  nameOf = cotypeConstructorEntryName

instance HasName (TraitEntry a) where
  nameOf = traitEntryName

instance HasName (Name, a) where
  nameOf = fst
