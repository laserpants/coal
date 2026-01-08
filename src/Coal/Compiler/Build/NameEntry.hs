{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
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
  dataConstructorEntries,
  codataAccessorEntries,
) where

import Coal.AST.Type.Parameterized (instantiateVars)
import Coal.Common.Environment (Environment (..))
import Coal.Language
import Coal.Language.Module (CotypeDefinition (..), TypeDefinition (..))
import Control.Monad.State (evalState)
import Data.Binary (Binary)
import qualified Data.Set as Set
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

type IndexedCodataAccessor = CodataAccessor TypeIndex Kind IndexedType

data CodataAccessorEntry a = CodataAccessorEntry
  { codataAccessorEntryMetadata :: a
  , codataAccessorEntryName :: Name
  , codataAccessorEntryAccessor :: IndexedCodataAccessor
  }
  deriving (Show, Eq, Ord, Read, Generic)

instance (Binary a) => Binary (CodataAccessorEntry a)

data TypeConstructorEntry a = TypeConstructorEntry
  { typeConstructorEntryMetadata :: a
  , typeConstructorEntryName :: Name
  , typeConstructorEntryKind :: Kind
  , typeConstructorEntryDataConstructors :: [Name]
  }
  deriving (Show, Eq, Ord, Read, Generic)

instance (Binary a) => Binary (TypeConstructorEntry a)

data CotypeConstructorEntry a = CotypeConstructorEntry
  { cotypeConstructorEntryMetadata :: a
  , cotypeConstructorEntryName :: Name
  , cotypeConstructorEntryKind :: Kind
  , cotypeConstructorEntryDataAccessors :: [Name]
  }
  deriving (Show, Eq, Ord, Read, Generic)

instance (Binary a) => Binary (CotypeConstructorEntry a)

data TraitEntry a = TraitEntry
  { traitEntryMetadata :: a
  , traitEntryName :: Name
  , traitEntryParameter :: Parameter Kind
  , traitEntryRequiredInstances :: [Trait (Parameter Kind)]
  , traitEntryEntries :: Environment (Scheme Parameter Kind (Type Parameter Kind))
  }
  deriving (Show, Eq, Ord, Read, Generic)

instance (Binary a) => Binary (TraitEntry a)

data InstanceEntry a = InstanceEntry
  { instanceEntryMetadata :: a
  , instanceEntryType :: ParameterizedType
  , instanceEntryIndexedType :: IndexedType
  , instanceEntryEntries :: Dictionary IndexedScheme
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
  | NUnfold Name IndexedScheme
  | NDataConstructor Name IndexedScheme
  | NCodataAccessor Name IndexedScheme
  | NType Name Kind
  | NCotype Name Kind
  | NTrait Name
  | NAlias Name
  | NFunctionPlaceholder Name
  | NConstantPlaceholder Name
  | NFoldPlaceholder Name
  | NUnfoldPlaceholder Name
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
      NUnfold name _ ->
        name
      NDataConstructor name _ ->
        name
      NCodataAccessor name _ ->
        name
      NType name _ ->
        name
      NCotype name _ ->
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
      NUnfoldPlaceholder name ->
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

dataConstructorEntries :: Environment Kind -> a -> TypeDefinition -> [DataConstructorEntry a]
dataConstructorEntries env loc (TypeDefinition _ ctors) = getEntry <$> ctors
 where
  getEntry DataConstructor{constructorName = name, ..} =
    DataConstructorEntry
      loc
      name
      DataConstructor
        { constructorName = name
        , constructorScheme = translateScheme env constructorScheme
        , ..
        }
      (Set.fromList (constructorName <$> ctors))

codataAccessorEntries :: Environment Kind -> a -> CotypeDefinition -> [CodataAccessorEntry a]
codataAccessorEntries env loc (CotypeDefinition _ xsors) = getEntry <$> xsors
 where
  getEntry CodataAccessor{..} =
    CodataAccessorEntry
      loc
      accessorName
      (CodataAccessor accessorName (translateScheme env accessorScheme))

translateScheme :: Environment Kind -> Scheme Parameter () ParameterizedType -> IndexedScheme
translateScheme env (Forall _ _ s) = Forall vs [] t
 where
  vs = typeIndexesIn t
  t = evalState (instantiateVars [] env s) (0 :: Int)
