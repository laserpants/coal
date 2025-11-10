{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Module.Bundle where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Transform.Type.Parameterized
import Coal.Language
import Coal.Language.Module
import Control.Monad.State (evalState)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Extras (Dictionary, Name, Set)

type IndexedConstructor = DataConstructor TypeIndex Kind IndexedType

data DataConstructorInfo a
  = DataConstructorInfo a Name IndexedConstructor (Set Name)
  deriving (Show, Eq, Ord, Read)

data CodataAccessorInfo a
  = CodataAccessorInfo a Name (CodataAccessor TypeIndex Kind IndexedType)
  deriving (Show, Eq, Ord, Read)

data TypeConstructorInfo a
  = TypeConstructorInfo a Name Kind
  deriving (Show, Eq, Ord, Read)

data CotypeConstructorInfo a
  = CotypeConstructorInfo a Name Kind
  deriving (Show, Eq, Ord, Read)

data TraitInfo a
  = TraitInfo a Name (Parameter Kind) (Environment (Scheme Parameter () ParameterizedType))
  deriving (Show, Eq, Ord, Read)

data InstanceInfo a
  = InstanceInfo a ParameterizedType (Dictionary IndexedScheme)
  deriving (Show, Eq, Ord, Read)

data AliasInfo a
  = AliasInfo a Name [Name] ParameterizedType
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

data ModuleBundle = ModuleBundle
  { modulePath :: Path
  , moduleDataConstructors :: Environment (DataConstructorInfo Metadata)
  , moduleCodataAccessors :: Environment (CodataAccessorInfo Metadata)
  , moduleTypeConstructors :: Environment (TypeConstructorInfo Metadata)
  , moduleCotypeConstructors :: Environment (CotypeConstructorInfo Metadata)
  , moduleTraits :: Environment (TraitInfo Metadata)
  , moduleInstances :: Environment (Map IndexedType (InstanceInfo Metadata))
  , moduleAliases :: Environment (AliasInfo Metadata)
  , moduleNames :: Environment NameInfo
  , moduleExports :: Set Name
  --  , moduleDefinitions ::
  --  , moduleObjectCode :: ByteString
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE exportsAll #-}
exportsAll :: Set Name -> Bool
exportsAll s = Set.fromList ["*"] == s

exportedNames :: ModuleBundle -> Environment NameInfo
exportedNames ModuleBundle{..}
  | exportsAll moduleExports = moduleNames
  | otherwise = Environment.filterNames (`Set.member` moduleExports) moduleNames

exportedTypeConstructors :: ModuleBundle -> Environment (TypeConstructorInfo Metadata)
exportedTypeConstructors ModuleBundle{..}
  | exportsAll moduleExports = moduleTypeConstructors
  | otherwise = Environment.filterNames (`Set.member` moduleExports) moduleTypeConstructors

exportedCotypeConstructors :: ModuleBundle -> Environment (CotypeConstructorInfo Metadata)
exportedCotypeConstructors ModuleBundle{..}
  | exportsAll moduleExports = moduleCotypeConstructors
  | otherwise = Environment.filterNames (`Set.member` moduleExports) moduleCotypeConstructors

exportedDataConstructors :: ModuleBundle -> Environment (DataConstructorInfo Metadata)
exportedDataConstructors ModuleBundle{..}
  | exportsAll moduleExports = moduleDataConstructors
  | otherwise = Environment.filterNames (`Set.member` moduleExports) moduleDataConstructors

exportedCodataAccessors :: ModuleBundle -> Environment (CodataAccessorInfo Metadata)
exportedCodataAccessors ModuleBundle{..}
  | exportsAll moduleExports = moduleCodataAccessors
  | otherwise = Environment.filterNames (`Set.member` moduleExports) moduleCodataAccessors

emptyModuleBundle :: ModuleBundle
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

insertDataConstructor :: Name -> DataConstructorInfo Metadata -> ModuleBundle -> ModuleBundle
insertDataConstructor name info ModuleBundle{..} =
  ModuleBundle
    { moduleDataConstructors =
        Environment.insert name info moduleDataConstructors
    , ..
    }

insertManyDataConstructors :: [DataConstructorInfo Metadata] -> ModuleBundle -> ModuleBundle
insertManyDataConstructors infos ModuleBundle{..} =
  ModuleBundle
    { moduleDataConstructors =
        Environment.insertMultiple
          [(name, info) | info@(DataConstructorInfo _ name _ _) <- infos]
          moduleDataConstructors
    , ..
    }

insertCodataAccessor :: Name -> CodataAccessorInfo Metadata -> ModuleBundle -> ModuleBundle
insertCodataAccessor name info ModuleBundle{..} =
  ModuleBundle
    { moduleCodataAccessors =
        Environment.insert name info moduleCodataAccessors
    , ..
    }

insertManyCodataAccessors :: [CodataAccessorInfo Metadata] -> ModuleBundle -> ModuleBundle
insertManyCodataAccessors infos ModuleBundle{..} =
  ModuleBundle
    { moduleCodataAccessors =
        Environment.insertMultiple
          [(name, info) | info@(CodataAccessorInfo _ name _) <- infos]
          moduleCodataAccessors
    , ..
    }

insertTypeConstructor :: Name -> TypeConstructorInfo Metadata -> ModuleBundle -> ModuleBundle
insertTypeConstructor name info ModuleBundle{..} =
  ModuleBundle
    { moduleTypeConstructors =
        Environment.insert name info moduleTypeConstructors
    , ..
    }

insertCotypeConstructor :: Name -> CotypeConstructorInfo Metadata -> ModuleBundle -> ModuleBundle
insertCotypeConstructor name info ModuleBundle{..} =
  ModuleBundle
    { moduleCotypeConstructors =
        Environment.insert name info moduleCotypeConstructors
    , ..
    }

insertTrait :: Name -> TraitInfo Metadata -> ModuleBundle -> ModuleBundle
insertTrait name info ModuleBundle{..} =
  ModuleBundle
    { moduleTraits =
        Environment.insert name info moduleTraits
    , ..
    }

insertInstance :: Name -> IndexedType -> InstanceInfo Metadata -> ModuleBundle -> ModuleBundle
insertInstance name t info ModuleBundle{..} =
  ModuleBundle
    { moduleInstances =
        Environment.insert name (Map.insert t info entries) moduleInstances
    , ..
    }
 where
  entries = fromMaybe mempty (Environment.lookup name moduleInstances)

insertAlias :: Name -> AliasInfo Metadata -> ModuleBundle -> ModuleBundle
insertAlias name info ModuleBundle{..} = ModuleBundle{moduleAliases = Environment.insert name info moduleAliases, ..}

addName :: Name -> NameInfo -> ModuleBundle -> ModuleBundle
addName name info ModuleBundle{..} = ModuleBundle{moduleNames = Environment.insert name info moduleNames, ..}

addExport :: Name -> ModuleBundle -> ModuleBundle
addExport name ModuleBundle{..} = ModuleBundle{moduleExports = Set.insert name moduleExports, ..}

setExports :: [Name] -> ModuleBundle -> ModuleBundle
setExports names ModuleBundle{..} = ModuleBundle{moduleExports = Set.fromList names, ..}

setPath :: Path -> ModuleBundle -> ModuleBundle
setPath path ModuleBundle{..} = ModuleBundle{modulePath = path, ..}

typeConstructorInfo :: Metadata -> Name -> TypeDef -> TypeConstructorInfo Metadata
typeConstructorInfo loc name (TypeDef ps _) = TypeConstructorInfo loc name (kind n) where n = length ps

cotypeConstructorInfo :: Metadata -> Name -> CotypeDef -> CotypeConstructorInfo Metadata
cotypeConstructorInfo loc name (CotypeDef ps _) = CotypeConstructorInfo loc name (kind n) where n = length ps

{-# INLINE kind #-}
kind :: Int -> Kind
kind n = foldr KArrow KType (replicate n KType)

dataConstructorInfo :: Environment Kind -> Metadata -> TypeDef -> [DataConstructorInfo Metadata]
dataConstructorInfo env loc (TypeDef _ ctors) = getInfo <$> ctors
 where
  allNames = Set.fromList (constructorName <$> ctors)

  getInfo :: DataConstructor Parameter () ParameterizedType -> DataConstructorInfo Metadata
  getInfo DataConstructor{..} =
    DataConstructorInfo
      loc
      constructorName
      DataConstructor{constructorScheme = translateScheme env constructorScheme, ..}
      allNames

codataAccessorInfo :: Environment Kind -> Metadata -> CotypeDef -> [CodataAccessorInfo Metadata]
codataAccessorInfo env loc (CotypeDef _ xsors) = getInfo <$> xsors
 where
  getInfo :: CodataAccessor Parameter () ParameterizedType -> CodataAccessorInfo Metadata
  getInfo CodataAccessor{..} =
    CodataAccessorInfo
      loc
      codataAccessorName
      (CodataAccessor codataAccessorName (translateScheme env codataAccessorScheme))

translateScheme :: Environment Kind -> Scheme Parameter () ParameterizedType -> Scheme TypeIndex Kind IndexedType
translateScheme env (Forall _ _ t) = Forall (typeIndexesIn t1) [] t1
 where
  t1 = evalState (instantiateVars [] env t) (0 :: Int)

traitInfo :: Metadata -> Name -> TraitDef () -> TraitInfo Metadata
traitInfo loc name (TraitDef _ p ps) = TraitInfo loc name p (Environment.fromList ps)

aliasInfo :: Metadata -> Name -> AliasDef -> AliasInfo Metadata
aliasInfo loc name (AliasDef ps t) = AliasInfo loc name (parameterName <$> ps) t

instanceInfo :: Metadata -> Environment IndexedScheme -> InstanceDef a Metadata Kind () -> InstanceInfo Metadata
instanceInfo loc (Environment e) (InstanceDef _ q _) = InstanceInfo loc q e

-- TODO
toIndexedScheme :: Environment Kind -> Parameter Kind -> Scheme Parameter () ParameterizedType -> Scheme TypeIndex Kind IndexedType
toIndexedScheme env p (Forall _ _ t) = scheme [] (toIndexedType env p t)

toIndexedType :: Environment Kind -> Parameter Kind -> Type Parameter () -> IndexedType
toIndexedType env (Parameter k n) t = evalState (instantiateVars [(n, TypeIndex k 0)] env t) (1 :: Int)
