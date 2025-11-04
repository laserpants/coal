{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Module.Bundle where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Control.Monad.State (StateT, execStateT, modify)
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Extras (Dictionary, Name, Set, forM_)

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
  | IType
  | ICotype
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
setExports names ModuleBundle{..} = ModuleBundle{moduleExports = Set.fromList names}

dataConstructorInfo :: Metadata -> TypeDef -> [DataConstructorInfo]
dataConstructorInfo = undefined

typeConstructorInfo :: Metadata -> Name -> TypeDef -> TypeConstructorInfo
typeConstructorInfo loc name (TypeDef ps _) = TypeConstructorInfo loc name kind
 where
  kind = foldr KArrow KType (replicate (length ps) KType)

cotypeConstructorInfo :: Metadata -> Name -> CotypeDef -> CotypeConstructorInfo
cotypeConstructorInfo loc name (CotypeDef ps _) = CotypeConstructorInfo loc name kind
 where
  kind = undefined -- foldr KArrow KCotype (replicate (length ps) KCotype)

codataAccessorInfo :: Metadata -> CotypeDef -> [CodataAccessorInfo]
codataAccessorInfo loc (CotypeDef ps ts) = undefined -- CodataAccessorInfo loc name accessor
 where
  accessor = CodataAccessor undefined undefined

traitInfo :: Metadata -> Name -> TraitDef () -> TraitInfo
traitInfo = undefined

instanceInfo :: Metadata -> Name -> InstanceDef Definition Metadata Kind () -> InstanceInfo
instanceInfo = undefined

aliasInfo :: Metadata -> Name -> AliasDef -> AliasInfo
aliasInfo = undefined

build :: (Monad m) => Module Metadata Kind () -> CompilerT Metadata m ModuleBundle
build (Module _ exports defs) =
  flip execStateT emptyModuleBundle $ do
    modify (setExports exports)
    forM_ defs buildDefinition

buildDefinition :: (Monad m) => Definition Metadata Kind () -> StateT ModuleBundle (CompilerT Metadata m) ()
buildDefinition =
  \case
    DType loc name def -> do
      modify (insertTypeConstructor name (typeConstructorInfo loc name def))
      modify (insertManyDataConstructors (dataConstructorInfo loc def))
    DCotype loc name def -> do
      modify (insertCotypeConstructor name (cotypeConstructorInfo loc name def))
      modify (insertManyCodataAccessors (codataAccessorInfo loc def))
    DFunction{} ->
      pure ()
    DConstant{} ->
      pure ()
    DTrait loc name t -> do
      modify (insertTrait name (traitInfo loc name t))
    DInstance loc name def -> do
      modify (insertInstance name (instanceInfo loc name def))
    DTypeAlias loc name alias -> do
      modify (insertAlias name (aliasInfo loc name alias))
    DFold loc name d ->
      pure ()
    DUnfold loc name d ->
      pure ()
    DImport{} ->
      pure ()
