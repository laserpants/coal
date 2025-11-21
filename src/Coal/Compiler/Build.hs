{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
  HasName (..),
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
  exportedNames,
  exportedTypeNames,
  traitInfo,
  setExports,
  setTypeExports,
  setPath,
) where

import Coal.AST.Type.Parameterized (instantiateVars)
import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build.NameInfo
import Coal.Language
import Coal.Language.Module (AliasDef (..), CotypeDef (..), Path (Path), TraitDef (..), TypeDef (..))
import Control.Monad.State (evalState)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Extras (Name, Set, for)

data ModuleBuild a = ModuleBuild
  { modulePath :: Path
  , moduleDataConstructors :: Environment (DataConstructorInfo a)
  , moduleCodataAccessors :: Environment (CodataAccessorInfo a)
  , moduleTypeConstructors :: Environment (TypeConstructorInfo a)
  , moduleCotypeConstructors :: Environment (CotypeConstructorInfo a)
  , moduleTraits :: Environment (TraitInfo a)
  , moduleInstances :: Environment (Map IndexedType (InstanceInfo a))
  , moduleAliases :: Environment (AliasInfo a)
  , moduleNames :: [NameInfo]
  , moduleExports :: Set Name
  , moduleTypeExports :: Set Name
  --  , moduleDefinitions ::
  --  , moduleObjectCode :: ByteString
  }
  deriving (Show, Eq, Ord, Read)

memberOf :: (HasName a) => Set Name -> a -> Bool
memberOf s info = nameOf info `Set.member` s

exportedNames :: ModuleBuild a -> [NameInfo]
exportedNames ModuleBuild{..} = filter (memberOf moduleExports) moduleNames

exportedTypeNames :: ModuleBuild a -> [NameInfo]
exportedTypeNames ModuleBuild{..} = filter (memberOf moduleTypeExports) moduleNames

exportedTypeConstructors :: ModuleBuild a -> [TypeConstructorInfo a]
exportedTypeConstructors ModuleBuild{..} = snd <$> filter (memberOf moduleTypeExports) (Environment.toList moduleTypeConstructors)

exportedCotypeConstructors :: ModuleBuild a -> [CotypeConstructorInfo a]
exportedCotypeConstructors ModuleBuild{..} = snd <$> filter (memberOf moduleTypeExports) (Environment.toList moduleCotypeConstructors)

exportedDataConstructors :: ModuleBuild a -> [DataConstructorInfo a]
exportedDataConstructors ModuleBuild{..} = snd <$> filter (memberOf moduleExports) (Environment.toList moduleDataConstructors)

exportedCodataAccessors :: ModuleBuild a -> [CodataAccessorInfo a]
exportedCodataAccessors ModuleBuild{..} = snd <$> filter (memberOf moduleExports) (Environment.toList moduleCodataAccessors)

exportedTraits :: ModuleBuild a -> [TraitInfo a]
exportedTraits ModuleBuild{..} = snd <$> filter (memberOf moduleTypeExports) (Environment.toList moduleTraits)

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

addName :: NameInfo -> ModuleBuild a -> ModuleBuild a
addName info ModuleBuild{..} = ModuleBuild{moduleNames = info : moduleNames, ..}

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
