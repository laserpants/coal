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
import Coal.Language.Module (CotypeDef (..), TypeDef (..))
import Control.Monad.State (evalState)
import qualified Data.Set as Set
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
  , traitEntryRequiredInstances :: [Trait (Parameter Kind)]
  , traitEntryEntries :: Environment (Scheme Parameter Kind (Type Parameter Kind))
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
  deriving (Show, Eq, Ord, Read)

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

dataConstructorEntries :: Environment Kind -> a -> TypeDef -> [DataConstructorEntry a]
dataConstructorEntries env loc (TypeDef _ ctors) = getEntry <$> ctors
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

codataAccessorEntries :: Environment Kind -> a -> CotypeDef -> [CodataAccessorEntry a]
codataAccessorEntries env loc (CotypeDef _ xsors) = getEntry <$> xsors
 where
  getEntry CodataAccessor{..} =
    CodataAccessorEntry
      loc
      codataAccessorName
      (CodataAccessor codataAccessorName (translateScheme env codataAccessorScheme))

-- TODO
translateScheme :: Environment Kind -> Scheme Parameter () ParameterizedType -> Scheme TypeIndex Kind IndexedType
translateScheme env (Forall _ _ s) = Forall (typeIndexesIn t) [] t
 where
  t = evalState (instantiateVars [] env s) (0 :: Int)
