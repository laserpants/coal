{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Module.Bundle (
  ModuleBundle (..),
  CotypeConstructorInfo (..),
  DataConstructorInfo (..),
  TypeConstructorInfo (..),
  CodataAccessorInfo (..),
  TraitInfo (..),
  InstanceInfo (..),
  AliasInfo (..),
  NameInfo (..),
  emptyModuleBundle,
  addName,
  addExport,
  toIndexedScheme,
  toIndexedType,
  dataConstructorInfo,
  codataAccessorInfo,
  cotypeConstructorInfo,
  typeConstructorInfo,
  aliasInfo,
  insertInstance,
  insertTrait,
  insertCodataAccessor,
  insertAlias,
  insertDataConstructor,
  insertCotypeConstructor,
  insertTypeConstructor,
  insertManyDataConstructors,
  insertManyCodataAccessors,
  exportedCotypeConstructors,
  exportedCodataAccessors,
  exportedDataConstructors,
  exportedTypeConstructors,
  exportedTraits,
  exportedInstances,
  exportedNames,
  traitInfo,
  setExports,
  setPath,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Ast.Type.Parameterized
import Coal.Language
import Coal.Language.Module
import Control.Monad.State (evalState)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Extras (Dictionary, Name, Set)

type IndexedConstructor = DataConstructor TypeIndex Kind IndexedType

data DataConstructorInfo a = DataConstructorInfo
  { dataConstructorInfoMetaData :: a
  , dataConstructorInfoName :: Name
  , dataConstructorInfoContructor :: IndexedConstructor
  , dataConstructorInfoNameSet :: Set Name
  }
  deriving (Show, Eq, Ord, Read)

data CodataAccessorInfo a = CodataAccessorInfo
  { codataAccessorInfoMetadata :: a
  , codataAccessorInfoName :: Name
  , codataAccessorInfoAccessor :: CodataAccessor TypeIndex Kind IndexedType
  }
  deriving (Show, Eq, Ord, Read)

data TypeConstructorInfo a = TypeConstructorInfo
  { typeConstructorInfoMetadata :: a
  , typeConstructorInfoName :: Name
  , typeConstructorInfoKind :: Kind
  }
  deriving (Show, Eq, Ord, Read)

data CotypeConstructorInfo a = CotypeConstructorInfo
  { cotypeConstructorInfoMetadata :: a
  , cotypeConstructorInfoName :: Name
  , cotypeConstructorInfoKind :: Kind
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
  = IFunction IndexedScheme
  | IConstant IndexedScheme
  | IFold IndexedScheme
  | IUnfold IndexedScheme
  | IDataConstructor IndexedScheme
  | ICodataAccessor IndexedScheme
  | IType Kind
  | ICotype Kind
  | ITrait
  | IAlias
  deriving (Show, Eq, Ord, Read)

data ModuleBundle a = ModuleBundle
  { modulePath :: Path
  , moduleDataConstructors :: Environment (DataConstructorInfo a)
  , moduleCodataAccessors :: Environment (CodataAccessorInfo a)
  , moduleTypeConstructors :: Environment (TypeConstructorInfo a)
  , moduleCotypeConstructors :: Environment (CotypeConstructorInfo a)
  , moduleTraits :: Environment (TraitInfo a)
  , moduleInstances :: Environment (Map IndexedType (InstanceInfo a))
  , moduleAliases :: Environment (AliasInfo a)
  , moduleNames :: Environment NameInfo
  , moduleExports :: Set Name
  --  , moduleDefinitions ::
  --  , moduleObjectCode :: ByteString
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE exportsAll #-}
exportsAll :: Set Name -> Bool
exportsAll s = Set.fromList ["*"] == s

exportedNames :: ModuleBundle a -> Environment NameInfo
exportedNames ModuleBundle{..}
  | exportsAll moduleExports = moduleNames
  | otherwise = Environment.filterNames (`Set.member` moduleExports) moduleNames

exportedTypeConstructors :: ModuleBundle a -> Environment (TypeConstructorInfo a)
exportedTypeConstructors ModuleBundle{..}
  | exportsAll moduleExports = moduleTypeConstructors
  | otherwise = Environment.filterNames (`Set.member` moduleExports) moduleTypeConstructors

exportedCotypeConstructors :: ModuleBundle a -> Environment (CotypeConstructorInfo a)
exportedCotypeConstructors ModuleBundle{..}
  | exportsAll moduleExports = moduleCotypeConstructors
  | otherwise = Environment.filterNames (`Set.member` moduleExports) moduleCotypeConstructors

exportedDataConstructors :: ModuleBundle a -> Environment (DataConstructorInfo a)
exportedDataConstructors ModuleBundle{..}
  | exportsAll moduleExports = moduleDataConstructors
  | otherwise = Environment.filterNames (`Set.member` moduleExports) moduleDataConstructors

exportedCodataAccessors :: ModuleBundle a -> Environment (CodataAccessorInfo a)
exportedCodataAccessors ModuleBundle{..}
  | exportsAll moduleExports = moduleCodataAccessors
  | otherwise = Environment.filterNames (`Set.member` moduleExports) moduleCodataAccessors

exportedTraits :: ModuleBundle a -> Environment (TraitInfo a)
exportedTraits ModuleBundle{..}
  | exportsAll moduleExports = moduleTraits
  | otherwise = Environment.filterNames (`Set.member` moduleExports) moduleTraits

exportedInstances :: ModuleBundle a -> Environment (Map IndexedType (InstanceInfo a))
exportedInstances ModuleBundle{..}
  | exportsAll moduleExports = moduleInstances
  | otherwise = Environment.filterNames (`Set.member` moduleExports) moduleInstances

emptyModuleBundle :: ModuleBundle a
emptyModuleBundle =
  ModuleBundle
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
    }

insertDataConstructor :: Name -> DataConstructorInfo a -> ModuleBundle a -> ModuleBundle a
insertDataConstructor name info ModuleBundle{..} =
  ModuleBundle
    { moduleDataConstructors =
        Environment.insert name info moduleDataConstructors
    , ..
    }

insertManyDataConstructors :: [DataConstructorInfo a] -> ModuleBundle a -> ModuleBundle a
insertManyDataConstructors infos ModuleBundle{..} =
  ModuleBundle
    { moduleDataConstructors =
        Environment.insertMultiple
          [(name, info) | info@(DataConstructorInfo _ name _ _) <- infos]
          moduleDataConstructors
    , ..
    }

insertCodataAccessor :: Name -> CodataAccessorInfo a -> ModuleBundle a -> ModuleBundle a
insertCodataAccessor name info ModuleBundle{..} =
  ModuleBundle
    { moduleCodataAccessors =
        Environment.insert name info moduleCodataAccessors
    , ..
    }

insertManyCodataAccessors :: [CodataAccessorInfo a] -> ModuleBundle a -> ModuleBundle a
insertManyCodataAccessors infos ModuleBundle{..} =
  ModuleBundle
    { moduleCodataAccessors =
        Environment.insertMultiple
          [(name, info) | info@(CodataAccessorInfo _ name _) <- infos]
          moduleCodataAccessors
    , ..
    }

insertTypeConstructor :: Name -> TypeConstructorInfo a -> ModuleBundle a -> ModuleBundle a
insertTypeConstructor name info ModuleBundle{..} =
  ModuleBundle
    { moduleTypeConstructors =
        Environment.insert name info moduleTypeConstructors
    , ..
    }

insertCotypeConstructor :: Name -> CotypeConstructorInfo a -> ModuleBundle a -> ModuleBundle a
insertCotypeConstructor name info ModuleBundle{..} =
  ModuleBundle
    { moduleCotypeConstructors =
        Environment.insert name info moduleCotypeConstructors
    , ..
    }

insertTrait :: Name -> TraitInfo a -> ModuleBundle a -> ModuleBundle a
insertTrait name info ModuleBundle{..} =
  ModuleBundle
    { moduleTraits =
        Environment.insert name info moduleTraits
    , ..
    }

insertInstance :: Name -> IndexedType -> InstanceInfo a -> ModuleBundle a -> ModuleBundle a
insertInstance name t info ModuleBundle{..} =
  ModuleBundle
    { moduleInstances =
        Environment.insert name (Map.insert t info entries) moduleInstances
    , ..
    }
 where
  entries = fromMaybe mempty (Environment.lookup name moduleInstances)

insertAlias :: Name -> AliasInfo a -> ModuleBundle a -> ModuleBundle a
insertAlias name info ModuleBundle{..} = ModuleBundle{moduleAliases = Environment.insert name info moduleAliases, ..}

addName :: Name -> NameInfo -> ModuleBundle a -> ModuleBundle a
addName name info ModuleBundle{..} = ModuleBundle{moduleNames = Environment.insert name info moduleNames, ..}

addExport :: Name -> ModuleBundle a -> ModuleBundle a
addExport name ModuleBundle{..} = ModuleBundle{moduleExports = Set.insert name moduleExports, ..}

setExports :: [Name] -> ModuleBundle a -> ModuleBundle a
setExports names ModuleBundle{..} = ModuleBundle{moduleExports = Set.fromList names, ..}

setPath :: Path -> ModuleBundle a -> ModuleBundle a
setPath path ModuleBundle{..} = ModuleBundle{modulePath = path, ..}

typeConstructorInfo :: a -> Name -> TypeDef -> TypeConstructorInfo a
typeConstructorInfo loc name (TypeDef ps _) = TypeConstructorInfo loc name (kind n) where n = length ps

cotypeConstructorInfo :: a -> Name -> CotypeDef -> CotypeConstructorInfo a
cotypeConstructorInfo loc name (CotypeDef ps _) = CotypeConstructorInfo loc name (kind n) where n = length ps

{-# INLINE kind #-}
kind :: Int -> Kind
kind n = foldr KArrow KType (replicate n KType)

dataConstructorInfo :: Environment Kind -> a -> TypeDef -> [DataConstructorInfo a]
dataConstructorInfo env loc (TypeDef _ ctors) = getInfo <$> ctors
 where
  allNames = Set.fromList (constructorName <$> ctors)

  getInfo DataConstructor{..} =
    DataConstructorInfo
      loc
      constructorName
      DataConstructor{constructorScheme = translateScheme env constructorScheme, ..}
      allNames

codataAccessorInfo :: Environment Kind -> a -> CotypeDef -> [CodataAccessorInfo a]
codataAccessorInfo env loc (CotypeDef _ xsors) = getInfo <$> xsors
 where
  getInfo CodataAccessor{..} =
    CodataAccessorInfo
      loc
      codataAccessorName
      (CodataAccessor codataAccessorName (translateScheme env codataAccessorScheme))

translateScheme :: Environment Kind -> Scheme Parameter () ParameterizedType -> Scheme TypeIndex Kind IndexedType
translateScheme env (Forall _ _ t) = Forall (typeIndexesIn t1) [] t1
 where
  t1 = evalState (instantiateVars [] env t) (0 :: Int)

traitInfo :: a -> Name -> TraitDef () -> TraitInfo a
traitInfo loc name (TraitDef _ p ps) = TraitInfo loc name p (Environment.fromList ps)

aliasInfo :: a -> Name -> AliasDef -> AliasInfo a
aliasInfo loc name (AliasDef ps t) = AliasInfo loc name (parameterName <$> ps) t

-- TODO
toIndexedScheme :: Environment Kind -> Parameter Kind -> Scheme Parameter () ParameterizedType -> Scheme TypeIndex Kind IndexedType
toIndexedScheme env p (Forall _ _ t) = scheme [] (toIndexedType env p t)

toIndexedType :: Environment Kind -> Parameter Kind -> Type Parameter () -> IndexedType
toIndexedType env (Parameter k n) t = evalState (instantiateVars [(n, TypeIndex k 0)] env t) (1 :: Int)
