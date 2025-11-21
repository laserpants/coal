{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Build (
  ModuleBuild (..),
  CotypeConstructorEntry (..),
  DataConstructorEntry (..),
  TypeConstructorEntry (..),
  CodataAccessorEntry (..),
  TraitEntry (..),
  InstanceEntry (..),
  AliasEntry (..),
  NameEntry (..),
  HasName (..),
  emptyModuleBuild,
  addName,
  addExport,
  addTypeExport,
  toIndexedScheme,
  toIndexedType,
  dataConstructorEntry,
  codataAccessorEntry,
  cotypeConstructorEntry,
  typeConstructorEntry,
  aliasEntry,
  traitEntry,
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
  setExports,
  setTypeExports,
  setPath,
) where

import Coal.AST.Type.Parameterized (instantiateVars)
import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build.NameEntry
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
  , moduleDataConstructors :: Environment (DataConstructorEntry a)
  , moduleCodataAccessors :: Environment (CodataAccessorEntry a)
  , moduleTypeConstructors :: Environment (TypeConstructorEntry a)
  , moduleCotypeConstructors :: Environment (CotypeConstructorEntry a)
  , moduleTraits :: Environment (TraitEntry a)
  , moduleInstances :: Environment (Map IndexedType (InstanceEntry a))
  , moduleAliases :: Environment (AliasEntry a)
  , moduleNames :: [NameEntry]
  , moduleExports :: Set Name
  , moduleTypeExports :: Set Name
  --  , moduleDefinitions ::
  --  , moduleObjectCode :: ByteString
  }
  deriving (Show, Eq, Ord, Read)

memberOf :: (HasName a) => Set Name -> a -> Bool
memberOf s info = nameOf info `Set.member` s

exportedNames :: ModuleBuild a -> [NameEntry]
exportedNames ModuleBuild{..} = filter (memberOf moduleExports) moduleNames

exportedTypeNames :: ModuleBuild a -> [NameEntry]
exportedTypeNames ModuleBuild{..} = filter (memberOf moduleTypeExports) moduleNames

exportedTypeConstructors :: ModuleBuild a -> [TypeConstructorEntry a]
exportedTypeConstructors ModuleBuild{..} = snd <$> filter (memberOf moduleTypeExports) (Environment.toList moduleTypeConstructors)

exportedCotypeConstructors :: ModuleBuild a -> [CotypeConstructorEntry a]
exportedCotypeConstructors ModuleBuild{..} = snd <$> filter (memberOf moduleTypeExports) (Environment.toList moduleCotypeConstructors)

exportedDataConstructors :: ModuleBuild a -> [DataConstructorEntry a]
exportedDataConstructors ModuleBuild{..} = snd <$> filter (memberOf moduleExports) (Environment.toList moduleDataConstructors)

exportedCodataAccessors :: ModuleBuild a -> [CodataAccessorEntry a]
exportedCodataAccessors ModuleBuild{..} = snd <$> filter (memberOf moduleExports) (Environment.toList moduleCodataAccessors)

exportedTraits :: ModuleBuild a -> [TraitEntry a]
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

insertDataConstructor :: Name -> DataConstructorEntry a -> ModuleBuild a -> ModuleBuild a
insertDataConstructor name info ModuleBuild{..} =
  ModuleBuild
    { moduleDataConstructors =
        Environment.insert name info moduleDataConstructors
    , ..
    }

insertManyDataConstructors :: [DataConstructorEntry a] -> ModuleBuild a -> ModuleBuild a
insertManyDataConstructors infos ModuleBuild{..} =
  ModuleBuild
    { moduleDataConstructors =
        Environment.insertMultiple
          [(name, info) | info@(DataConstructorEntry _ name _ _) <- infos]
          moduleDataConstructors
    , ..
    }

insertCodataAccessor :: Name -> CodataAccessorEntry a -> ModuleBuild a -> ModuleBuild a
insertCodataAccessor name info ModuleBuild{..} =
  ModuleBuild
    { moduleCodataAccessors =
        Environment.insert name info moduleCodataAccessors
    , ..
    }

insertManyCodataAccessors :: [CodataAccessorEntry a] -> ModuleBuild a -> ModuleBuild a
insertManyCodataAccessors infos ModuleBuild{..} =
  ModuleBuild
    { moduleCodataAccessors =
        Environment.insertMultiple
          [(name, info) | info@(CodataAccessorEntry _ name _) <- infos]
          moduleCodataAccessors
    , ..
    }

insertTypeConstructor :: Name -> TypeConstructorEntry a -> ModuleBuild a -> ModuleBuild a
insertTypeConstructor name info ModuleBuild{..} =
  ModuleBuild
    { moduleTypeConstructors =
        Environment.insert name info moduleTypeConstructors
    , ..
    }

insertCotypeConstructor :: Name -> CotypeConstructorEntry a -> ModuleBuild a -> ModuleBuild a
insertCotypeConstructor name info ModuleBuild{..} =
  ModuleBuild
    { moduleCotypeConstructors =
        Environment.insert name info moduleCotypeConstructors
    , ..
    }

insertTrait :: Name -> TraitEntry a -> ModuleBuild a -> ModuleBuild a
insertTrait name info ModuleBuild{..} =
  ModuleBuild
    { moduleTraits =
        Environment.insert name info moduleTraits
    , ..
    }

insertInstance :: Name -> IndexedType -> InstanceEntry a -> ModuleBuild a -> ModuleBuild a
insertInstance name t info ModuleBuild{..} =
  ModuleBuild
    { moduleInstances =
        Environment.insert name (Map.insert t info entries) moduleInstances
    , ..
    }
 where
  entries = fromMaybe mempty (Environment.lookup name moduleInstances)

insertAlias :: Name -> AliasEntry a -> ModuleBuild a -> ModuleBuild a
insertAlias name info ModuleBuild{..} = ModuleBuild{moduleAliases = Environment.insert name info moduleAliases, ..}

addName :: NameEntry -> ModuleBuild a -> ModuleBuild a
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

dataConstructorEntry :: Environment Kind -> a -> TypeDef -> [DataConstructorEntry a]
dataConstructorEntry env loc (TypeDef _ ctors) = getEntry <$> ctors
 where
  allNames = Set.fromList (constructorName <$> ctors)

  getEntry DataConstructor{..} =
    DataConstructorEntry
      loc
      constructorName
      DataConstructor{constructorScheme = translateScheme env constructorScheme, ..}
      allNames

codataAccessorEntry :: Environment Kind -> a -> CotypeDef -> [CodataAccessorEntry a]
codataAccessorEntry env loc (CotypeDef _ xsors) = getEntry <$> xsors
 where
  getEntry CodataAccessor{..} =
    CodataAccessorEntry
      loc
      codataAccessorName
      (CodataAccessor codataAccessorName (translateScheme env codataAccessorScheme))

translateScheme :: Environment Kind -> Scheme Parameter () ParameterizedType -> Scheme TypeIndex Kind IndexedType
translateScheme env (Forall _ _ t) = Forall (typeIndexesIn t1) [] t1
 where
  t1 = evalState (instantiateVars [] env t) (0 :: Int)

traitEntry :: a -> Name -> TraitDef () -> TraitEntry a
traitEntry loc name (TraitDef _ p ps) = TraitEntry loc name p (Environment.fromList ps)

aliasEntry :: a -> Name -> AliasDef -> AliasEntry a
aliasEntry loc name (AliasDef ps t) = AliasEntry loc name (parameterName <$> ps) t

-- TODO
toIndexedScheme :: Environment Kind -> Parameter Kind -> Scheme Parameter () ParameterizedType -> Scheme TypeIndex Kind IndexedType
toIndexedScheme env p (Forall _ _ t) = scheme [] (toIndexedType env p t)

toIndexedType :: Environment Kind -> Parameter Kind -> Type Parameter () -> IndexedType
toIndexedType env (Parameter k n) t = evalState (instantiateVars [(n, TypeIndex k 0)] env t) (1 :: Int)
