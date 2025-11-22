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
  aliasEntry,
  traitEntry,
  dataConstructorEntries,
  codataAccessorEntries,
  cotypeConstructorEntry,
  typeConstructorEntry,
) where

import Coal.AST.Type.Parameterized (instantiateVars)
import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Language
import Coal.Language.Module (AliasDef (..), CotypeDef (..), TraitDef (..), TypeDef (..))
import Control.Monad.State (evalState)
import qualified Data.Set as Set
import Extras (Dictionary, Name, Set, for)

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

typeConstructorEntry :: a -> Name -> TypeDef -> TypeConstructorEntry a
typeConstructorEntry loc name (TypeDef ps ctors) =
  TypeConstructorEntry loc name (kind n) (for ctors constructorName)
 where
  n = length ps

cotypeConstructorEntry :: a -> Name -> CotypeDef -> CotypeConstructorEntry a
cotypeConstructorEntry loc name (CotypeDef ps xsors) =
  CotypeConstructorEntry loc name (kind n) (for xsors codataAccessorName)
 where
  n = length ps

{-# INLINE kind #-}
kind :: Int -> Kind
kind n = foldr KArrow KType (replicate n KType)

dataConstructorEntries :: Environment Kind -> a -> TypeDef -> [DataConstructorEntry a]
dataConstructorEntries env loc (TypeDef _ ctors) = getEntry <$> ctors
 where
  allNames = Set.fromList (constructorName <$> ctors)

  getEntry DataConstructor{..} =
    DataConstructorEntry
      loc
      constructorName
      DataConstructor{constructorScheme = translateScheme env constructorScheme, ..}
      allNames

codataAccessorEntries :: Environment Kind -> a -> CotypeDef -> [CodataAccessorEntry a]
codataAccessorEntries env loc (CotypeDef _ xsors) = getEntry <$> xsors
 where
  getEntry CodataAccessor{..} =
    CodataAccessorEntry
      loc
      codataAccessorName
      (CodataAccessor codataAccessorName (translateScheme env codataAccessorScheme))

traitEntry :: a -> Name -> TraitDef () -> TraitEntry a
traitEntry loc name (TraitDef _ p ps) = TraitEntry loc name p (Environment.fromList ps)

aliasEntry :: a -> Name -> AliasDef -> AliasEntry a
aliasEntry loc name (AliasDef ps t) = AliasEntry loc name (parameterName <$> ps) t

translateScheme :: Environment Kind -> Scheme Parameter () ParameterizedType -> Scheme TypeIndex Kind IndexedType
translateScheme env (Forall _ _ s) = Forall (typeIndexesIn t) [] t
 where
  t = evalState (instantiateVars [] env s) (0 :: Int)
