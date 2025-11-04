{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Module.Bundle where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Control.Monad.State (StateT, modify, execStateT)
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
    , moduleTraits = mempty
    , moduleInstances = mempty
    , moduleAliases = mempty
    , moduleNames = mempty
    , moduleExports = mempty
    }

insertDataConstructor :: Name -> DataConstructorInfo -> ModuleBundle -> ModuleBundle
insertDataConstructor name info ModuleBundle{..} = ModuleBundle{moduleDataConstructors = Environment.insert name info moduleDataConstructors, ..}

insertCodataAccessor :: Name -> CodataAccessorInfo -> ModuleBundle -> ModuleBundle
insertCodataAccessor name info ModuleBundle{..} = ModuleBundle{moduleCodataAccessors = Environment.insert name info moduleCodataAccessors, ..}

insertTypeConstructor :: Name -> TypeConstructorInfo -> ModuleBundle -> ModuleBundle
insertTypeConstructor name info ModuleBundle{..} = ModuleBundle{moduleTypeConstructors = Environment.insert name info moduleTypeConstructors, ..}

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

typeConstructorInfo :: Metadata -> Name -> TypeDef -> TypeConstructorInfo
typeConstructorInfo loc name (TypeDef ps _) = TypeConstructorInfo loc name kind
 where
  kind = foldr KArrow KType (replicate (length ps) KType)

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
    DCotype loc name d -> do
      modify (insertCodataAccessor name undefined)
    DFunction loc name fs ds -> do
      undefined
    DConstant loc name d ds -> do
      undefined
    DTrait loc name t -> do
      undefined
    DInstance loc name d -> do
      undefined
    DTypeAlias loc name a -> do
      undefined
    DFold loc name d -> do
      undefined
    DUnfold loc name d -> do
      undefined
    DImport loc path ns -> do
      pure ()
