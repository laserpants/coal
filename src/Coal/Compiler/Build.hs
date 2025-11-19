{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Build (
  ModuleBuild (..),
  CotypeConstructorInfo (..),
  DataConstructorInfo (..),
  TypeConstructorInfo (..),
  CodataAccessorInfo (..),
  TraitInfo (..),
  InstanceInfo (..),
  AliasInfo (..),
  NameInfo (..),
  emptyModuleBuild,
  addName,
  addExport,
  addTypeExport,
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
  exportedTypeNames,
  traitInfo,
  setExports,
  setTypeExports,
  setPath,
) where

import Coal.AST.Type.Parameterized
import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Language
import Coal.Language.Module
import Control.Monad.State (evalState)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Extras (Dictionary, Name, Set, for)

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
  | IFunctionPlaceholder
  | IConstantPlaceholder
  | IFoldPlaceholder
  | IUnfoldPlaceholder
  deriving (Show, Eq, Ord, Read)

data ModuleBuild a = ModuleBuild
  { modulePath :: Path
  , moduleDataConstructors :: Environment (DataConstructorInfo a)
  , moduleCodataAccessors :: Environment (CodataAccessorInfo a)
  , moduleTypeConstructors :: Environment (TypeConstructorInfo a)
  , moduleCotypeConstructors :: Environment (CotypeConstructorInfo a)
  , moduleTraits :: Environment (TraitInfo a)
  , moduleInstances :: Environment (Map IndexedType (InstanceInfo a))
  , moduleAliases :: Environment (AliasInfo a)
  , moduleNames :: Environment NameInfo -- TODO : Make list?
  , moduleExports :: Set Name
  , moduleTypeExports :: Set Name
  --  , moduleDefinitions ::
  --  , moduleObjectCode :: ByteString
  }
  deriving (Show, Eq, Ord, Read)

exportedNames :: ModuleBuild a -> Environment NameInfo
exportedNames ModuleBuild{..} = Environment.filterNames (`Set.member` moduleExports) moduleNames

exportedTypeNames :: ModuleBuild a -> Environment NameInfo
exportedTypeNames ModuleBuild{..} = Environment.filterNames (`Set.member` moduleTypeExports) moduleNames

exportedTypeConstructors :: ModuleBuild a -> Environment (TypeConstructorInfo a)
exportedTypeConstructors ModuleBuild{..} = Environment.filterNames (`Set.member` moduleTypeExports) moduleTypeConstructors

exportedCotypeConstructors :: ModuleBuild a -> Environment (CotypeConstructorInfo a)
exportedCotypeConstructors ModuleBuild{..} = Environment.filterNames (`Set.member` moduleTypeExports) moduleCotypeConstructors

exportedDataConstructors :: ModuleBuild a -> Environment (DataConstructorInfo a)
exportedDataConstructors ModuleBuild{..} = Environment.filterNames (`Set.member` moduleExports) moduleDataConstructors

exportedCodataAccessors :: ModuleBuild a -> Environment (CodataAccessorInfo a)
exportedCodataAccessors ModuleBuild{..} = Environment.filterNames (`Set.member` moduleExports) moduleCodataAccessors

exportedTraits :: ModuleBuild a -> Environment (TraitInfo a)
exportedTraits ModuleBuild{..} = Environment.filterNames (`Set.member` moduleTypeExports) moduleTraits

exportedInstances :: ModuleBuild a -> Environment (Map IndexedType (InstanceInfo a))
exportedInstances ModuleBuild{..} = Environment.filterNames (`Set.member` moduleExports) moduleInstances

emptyModuleBuild :: ModuleBuild a
emptyModuleBuild =
  ModuleBuild
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
    , moduleTypeExports = mempty
    }

insertDataConstructor :: Name -> DataConstructorInfo a -> ModuleBuild a -> ModuleBuild a
insertDataConstructor name info ModuleBuild{..} =
  ModuleBuild
    { moduleDataConstructors =
        Environment.insert name info moduleDataConstructors
    , ..
    }

insertManyDataConstructors :: [DataConstructorInfo a] -> ModuleBuild a -> ModuleBuild a
insertManyDataConstructors infos ModuleBuild{..} =
  ModuleBuild
    { moduleDataConstructors =
        Environment.insertMultiple
          [(name, info) | info@(DataConstructorInfo _ name _ _) <- infos]
          moduleDataConstructors
    , ..
    }

insertCodataAccessor :: Name -> CodataAccessorInfo a -> ModuleBuild a -> ModuleBuild a
insertCodataAccessor name info ModuleBuild{..} =
  ModuleBuild
    { moduleCodataAccessors =
        Environment.insert name info moduleCodataAccessors
    , ..
    }

insertManyCodataAccessors :: [CodataAccessorInfo a] -> ModuleBuild a -> ModuleBuild a
insertManyCodataAccessors infos ModuleBuild{..} =
  ModuleBuild
    { moduleCodataAccessors =
        Environment.insertMultiple
          [(name, info) | info@(CodataAccessorInfo _ name _) <- infos]
          moduleCodataAccessors
    , ..
    }

insertTypeConstructor :: Name -> TypeConstructorInfo a -> ModuleBuild a -> ModuleBuild a
insertTypeConstructor name info ModuleBuild{..} =
  ModuleBuild
    { moduleTypeConstructors =
        Environment.insert name info moduleTypeConstructors
    , ..
    }

insertCotypeConstructor :: Name -> CotypeConstructorInfo a -> ModuleBuild a -> ModuleBuild a
insertCotypeConstructor name info ModuleBuild{..} =
  ModuleBuild
    { moduleCotypeConstructors =
        Environment.insert name info moduleCotypeConstructors
    , ..
    }

insertTrait :: Name -> TraitInfo a -> ModuleBuild a -> ModuleBuild a
insertTrait name info ModuleBuild{..} =
  ModuleBuild
    { moduleTraits =
        Environment.insert name info moduleTraits
    , ..
    }

insertInstance :: Name -> IndexedType -> InstanceInfo a -> ModuleBuild a -> ModuleBuild a
insertInstance name t info ModuleBuild{..} =
  ModuleBuild
    { moduleInstances =
        Environment.insert name (Map.insert t info entries) moduleInstances
    , ..
    }
 where
  entries = fromMaybe mempty (Environment.lookup name moduleInstances)

insertAlias :: Name -> AliasInfo a -> ModuleBuild a -> ModuleBuild a
insertAlias name info ModuleBuild{..} = ModuleBuild{moduleAliases = Environment.insert name info moduleAliases, ..}

addName :: Name -> NameInfo -> ModuleBuild a -> ModuleBuild a
addName name info ModuleBuild{..} = ModuleBuild{moduleNames = Environment.insert name info moduleNames, ..}

addExport :: Name -> ModuleBuild a -> ModuleBuild a
addExport name ModuleBuild{..} = ModuleBuild{moduleExports = Set.insert name moduleExports, ..}

addTypeExport :: Name -> ModuleBuild a -> ModuleBuild a
addTypeExport name ModuleBuild{..} = ModuleBuild{moduleTypeExports = Set.insert name moduleTypeExports, ..}

setExports :: [Name] -> ModuleBuild a -> ModuleBuild a
setExports names ModuleBuild{..} = ModuleBuild{moduleExports = Set.fromList names, ..}

setTypeExports :: [Name] -> ModuleBuild a -> ModuleBuild a
setTypeExports names ModuleBuild{..} = ModuleBuild{moduleTypeExports = Set.fromList names, ..}

setPath :: Path -> ModuleBuild a -> ModuleBuild a
setPath path ModuleBuild{..} = ModuleBuild{modulePath = path, ..}

typeConstructorInfo :: a -> Name -> TypeDef -> TypeConstructorInfo a
typeConstructorInfo loc name (TypeDef ps ctors) =
  TypeConstructorInfo loc name (kind n) (for ctors constructorName)
 where
  n = length ps

cotypeConstructorInfo :: a -> Name -> CotypeDef -> CotypeConstructorInfo a
cotypeConstructorInfo loc name (CotypeDef ps xsors) =
  CotypeConstructorInfo loc name (kind n) (for xsors codataAccessorName)
 where
  n = length ps

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
