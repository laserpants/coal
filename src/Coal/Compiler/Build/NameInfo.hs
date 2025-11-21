{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Build.NameInfo (
  DataConstructorInfo (..),
  CodataAccessorInfo (..),
  TypeConstructorInfo (..),
  CotypeConstructorInfo (..),
  TraitInfo (..),
  InstanceInfo (..),
  AliasInfo (..),
  NameInfo (..),
  HasName (..),
) where

import Coal.Common.Environment (Environment (..))
import Coal.Language
import Extras (Dictionary, Name, Set)

type IndexedConstructor = DataConstructor TypeIndex Kind IndexedType

data DataConstructorInfo a = DataConstructorInfo
  { dataConstructorInfoMetaData :: a
  , dataConstructorInfoName :: Name
  , dataConstructorInfoContructor :: IndexedConstructor
  , dataConstructorInfoNameSet :: Set Name
  }
  deriving (Show, Eq, Ord, Read)

type IndexedCodataAccessor = CodataAccessor TypeIndex Kind IndexedType

data CodataAccessorInfo a = CodataAccessorInfo
  { codataAccessorInfoMetadata :: a
  , codataAccessorInfoName :: Name
  , codataAccessorInfoAccessor :: IndexedCodataAccessor
  }
  deriving (Show, Eq, Ord, Read)

data TypeConstructorInfo a = TypeConstructorInfo
  { typeConstructorInfoMetadata :: a
  , typeConstructorInfoName :: Name
  , typeConstructorInfoKind :: Kind
  , typeConstructorInfoDataConstructors :: [Name]
  }
  deriving (Show, Eq, Ord, Read)

data CotypeConstructorInfo a = CotypeConstructorInfo
  { cotypeConstructorInfoMetadata :: a
  , cotypeConstructorInfoName :: Name
  , cotypeConstructorInfoKind :: Kind
  , cotypeConstructorInfoDataAccessors :: [Name]
  }
  deriving (Show, Eq, Ord, Read)

data TraitInfo a = TraitInfo
  { traitInfoMetadata :: a
  , traitInfoName :: Name
  , traitInfoParameter :: Parameter Kind
  , traitInfoEntries :: Environment (Scheme Parameter () ParameterizedType)
  }
  deriving (Show, Eq, Ord, Read)

data InstanceInfo a = InstanceInfo
  { instanceInfoMetadata :: a
  , instanceInfoType :: ParameterizedType
  , instanceInfoIndexedType :: IndexedType
  , instanceInfoEntries :: Dictionary IndexedScheme
  }
  deriving (Show, Eq, Ord, Read)

data AliasInfo a = AliasInfo
  { aliasInfoMetadata :: a
  , aliasInfoName :: Name
  , aliasInfoParams :: [Name]
  , aliasInfoType :: ParameterizedType
  }
  deriving (Show, Eq, Ord, Read)

data NameInfo
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

instance HasName NameInfo where
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

instance HasName (DataConstructorInfo a) where
  nameOf = dataConstructorInfoName

instance HasName (CodataAccessorInfo a) where
  nameOf = codataAccessorInfoName

instance HasName (TypeConstructorInfo a) where
  nameOf = typeConstructorInfoName

instance HasName (CotypeConstructorInfo a) where
  nameOf = cotypeConstructorInfoName

instance HasName (TraitInfo a) where
  nameOf = traitInfoName

instance HasName (Name, a) where
  nameOf = fst
