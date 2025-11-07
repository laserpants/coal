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
import Coal.TypeSystem.Substitution
import Control.Monad.State (evalState)
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Extras (Dictionary, Name, Set)

type IndexedConstructor = DataConstructor TypeIndex Kind IndexedType

data DataConstructorInfo
  = DataConstructorInfo Metadata Name IndexedConstructor (Set Name)
  deriving (Show, Eq, Ord, Read)

data CodataAccessorInfo
  = CodataAccessorInfo Metadata Name (CodataAccessor TypeIndex Kind IndexedType)
  deriving (Show, Eq, Ord, Read)

data TypeConstructorInfo
  = TypeConstructorInfo Metadata Name Kind
  deriving (Show, Eq, Ord, Read)

data CotypeConstructorInfo
  = CotypeConstructorInfo Metadata Name Kind
  deriving (Show, Eq, Ord, Read)

data TraitInfo
  = TraitInfo Metadata Name (Parameter Kind) (TypeIndex Kind) (Environment IndexedScheme)
  deriving (Show, Eq, Ord, Read)

data InstanceInfo
  = InstanceInfo Metadata Name (Map IndexedType ParameterizedType) (Dictionary IndexedScheme)
  deriving (Show, Eq, Ord, Read)

data AliasInfo
  = AliasInfo Metadata Name [Name] ParameterizedType
  deriving (Show, Eq, Ord, Read)

data NameInfo
  = IFunction IndexedScheme
  | IConstant IndexedScheme
  | IFold IndexedScheme
  | IUnfold IndexedScheme
  | IDataConstructor IndexedScheme
  | ICodataAccessor IndexedScheme
  | ITraitDefinition IndexedScheme
  | IType Kind
  | ICotype Kind
  | ITrait
  | IAlias
  deriving (Show, Eq, Ord, Read)

data ModuleBundle = ModuleBundle
  { moduleDataConstructors :: Environment DataConstructorInfo
  , moduleCodataAccessors :: Environment CodataAccessorInfo
  , moduleTypeConstructors :: Environment TypeConstructorInfo
  , moduleCotypeConstructors :: Environment CotypeConstructorInfo
  , moduleTraits :: Environment TraitInfo
  , moduleInstances :: Environment InstanceInfo
  , moduleAliases :: Environment AliasInfo
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

exportedTypeConstructors :: ModuleBundle -> Environment TypeConstructorInfo
exportedTypeConstructors ModuleBundle{..}
  | exportsAll moduleExports = moduleTypeConstructors
  | otherwise = Environment.filterNames (`Set.member` moduleExports) moduleTypeConstructors

exportedCotypeConstructors :: ModuleBundle -> Environment CotypeConstructorInfo
exportedCotypeConstructors ModuleBundle{..}
  | exportsAll moduleExports = moduleCotypeConstructors
  | otherwise = Environment.filterNames (`Set.member` moduleExports) moduleCotypeConstructors

exportedDataConstructors :: ModuleBundle -> Environment DataConstructorInfo
exportedDataConstructors ModuleBundle{..}
  | exportsAll moduleExports = moduleDataConstructors
  | otherwise = Environment.filterNames (`Set.member` moduleExports) moduleDataConstructors

exportedCodataAccessors :: ModuleBundle -> Environment CodataAccessorInfo
exportedCodataAccessors ModuleBundle{..}
  | exportsAll moduleExports = moduleCodataAccessors
  | otherwise = Environment.filterNames (`Set.member` moduleExports) moduleCodataAccessors

emptyModuleBundle :: ModuleBundle
emptyModuleBundle =
  ModuleBundle
    { moduleDataConstructors = mempty
    , moduleCodataAccessors = mempty
    , moduleTypeConstructors = mempty
    , moduleCotypeConstructors = mempty
    , moduleTraits = mempty
    , moduleInstances = mempty
    , moduleAliases = mempty
    , moduleNames = mempty
    , moduleExports = mempty
    }

insertDataConstructor :: Name -> DataConstructorInfo -> ModuleBundle -> ModuleBundle
insertDataConstructor name info ModuleBundle{..} = ModuleBundle{moduleDataConstructors = Environment.insert name info moduleDataConstructors, ..}

insertManyDataConstructors :: [DataConstructorInfo] -> ModuleBundle -> ModuleBundle
insertManyDataConstructors infos ModuleBundle{..} =
  ModuleBundle
    { moduleDataConstructors =
        Environment.insertMultiple
          [(name, info) | info@(DataConstructorInfo _ name _ _) <- infos]
          moduleDataConstructors
    , ..
    }

insertCodataAccessor :: Name -> CodataAccessorInfo -> ModuleBundle -> ModuleBundle
insertCodataAccessor name info ModuleBundle{..} = ModuleBundle{moduleCodataAccessors = Environment.insert name info moduleCodataAccessors, ..}

insertManyCodataAccessors :: [CodataAccessorInfo] -> ModuleBundle -> ModuleBundle
insertManyCodataAccessors infos ModuleBundle{..} =
  ModuleBundle
    { moduleCodataAccessors =
        Environment.insertMultiple
          [(name, info) | info@(CodataAccessorInfo _ name _) <- infos]
          moduleCodataAccessors
    , ..
    }

insertTypeConstructor :: Name -> TypeConstructorInfo -> ModuleBundle -> ModuleBundle
insertTypeConstructor name info ModuleBundle{..} = ModuleBundle{moduleTypeConstructors = Environment.insert name info moduleTypeConstructors, ..}

insertCotypeConstructor :: Name -> CotypeConstructorInfo -> ModuleBundle -> ModuleBundle
insertCotypeConstructor name info ModuleBundle{..} = ModuleBundle{moduleCotypeConstructors = Environment.insert name info moduleCotypeConstructors, ..}

insertTrait :: Name -> TraitInfo -> ModuleBundle -> ModuleBundle
insertTrait name info ModuleBundle{..} = ModuleBundle{moduleTraits = Environment.insert name info moduleTraits, ..}

insertInstance :: Name -> InstanceInfo -> ModuleBundle -> ModuleBundle
insertInstance name info ModuleBundle{..} = ModuleBundle{moduleInstances = Environment.insert name info moduleInstances, ..}

insertAlias :: Name -> AliasInfo -> ModuleBundle -> ModuleBundle
insertAlias name info ModuleBundle{..} = ModuleBundle{moduleAliases = Environment.insert name info moduleAliases, ..}

addName :: Name -> NameInfo -> ModuleBundle -> ModuleBundle
addName name info ModuleBundle{..} = ModuleBundle{moduleNames = Environment.insert name info moduleNames, ..}

addExport :: Name -> ModuleBundle -> ModuleBundle
addExport name ModuleBundle{..} = ModuleBundle{moduleExports = Set.insert name moduleExports, ..}

setExports :: [Name] -> ModuleBundle -> ModuleBundle
setExports names ModuleBundle{..} = ModuleBundle{moduleExports = Set.fromList names, ..}

typeConstructorInfo :: Metadata -> Name -> TypeDef -> TypeConstructorInfo
typeConstructorInfo loc name (TypeDef ps _) = TypeConstructorInfo loc name (kind n) where n = length ps

cotypeConstructorInfo :: Metadata -> Name -> CotypeDef -> CotypeConstructorInfo
cotypeConstructorInfo loc name (CotypeDef ps _) = CotypeConstructorInfo loc name (kind n) where n = length ps

{-# INLINE kind #-}
kind :: Int -> Kind
kind n = foldr KArrow KType (replicate n KType)

dataConstructorInfo :: Environment Kind -> Metadata -> TypeDef -> [DataConstructorInfo]
dataConstructorInfo env loc (TypeDef _ ctors) = getInfo <$> ctors
 where
  allNames = Set.fromList (constructorName <$> ctors)

  getInfo :: DataConstructor Parameter () ParameterizedType -> DataConstructorInfo
  getInfo DataConstructor{..} =
    DataConstructorInfo
      loc
      constructorName
      DataConstructor{constructorScheme = translateScheme env constructorScheme, ..}
      allNames

translateScheme :: Environment Kind -> Scheme Parameter () ParameterizedType -> Scheme TypeIndex Kind IndexedType
translateScheme env (Forall _ _ t) = Forall (typeIndexesIn t1) [] t1
 where
  t1 = evalState (instantiateVars [] env t) (0 :: Int)

codataAccessorInfo :: Environment Kind -> Metadata -> CotypeDef -> [CodataAccessorInfo]
codataAccessorInfo env loc (CotypeDef ps ts) = getInfo <$> undefined -- CodataAccessorInfo loc name accessor
 where
  getInfo :: CodataAccessor o k t -> CodataAccessorInfo
  getInfo = undefined

--  accessor = CodataAccessor undefined undefined

traitInfo :: Metadata -> Name -> TraitDef () -> TraitInfo
traitInfo = undefined

instanceInfo :: Metadata -> Name -> InstanceDef Definition Metadata Kind () -> InstanceInfo
instanceInfo = undefined

aliasInfo :: Metadata -> Name -> AliasDef -> AliasInfo
aliasInfo loc name (AliasDef ps t) = AliasInfo loc name (parameterName <$> ps) t
