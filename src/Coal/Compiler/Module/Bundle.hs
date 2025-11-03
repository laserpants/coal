{-# LANGUAGE LambdaCase #-}

module Coal.Compiler.Module.Bundle where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Common.Environment (Environment (..))
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Data.Map.Strict (Map)
import Extras (Dictionary, Name, Set, forM_)

type IndexedConstructor = DataConstructor TypeIndex Kind IndexedType

data DataConstructorInfo
  = DataConstructorInfo IndexedConstructor (Set Name)
  deriving (Show, Eq, Ord, Read)

data TraitInfo
  = TraitInfo
      (Parameter Kind)
      (TypeIndex Kind)
      (Environment IndexedScheme)
  deriving (Show, Eq, Ord, Read)

data InstanceInfo
  = InstanceInfo
      (Map IndexedType ParameterizedType)
      (Dictionary IndexedScheme)
  deriving (Show, Eq, Ord, Read)

data AliasInfo
  = AliasInfo [Name] ParameterizedType
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
  deriving (Show, Eq, Ord, Read)

data ModuleBundle = ModuleBundle
  { moduleDataConstructors :: Environment DataConstructorInfo
  , moduleCodataAccessors :: Environment (CodataAccessor TypeIndex Kind IndexedType)
  , moduleTypeConstructors :: Environment Kind
  , moduleTraits :: Environment TraitInfo
  , moduleInstances :: Environment InstanceInfo
  , moduleAliases :: Environment AliasInfo
  , moduleNames :: Environment NameInfo
  , moduleExports :: Set Name
  --  , moduleDefinitions ::
  --  , moduleObjectCode :: ByteString
  }
  deriving (Show, Eq, Ord, Read)

build :: (Monad m) => Module Metadata Kind () -> CompilerT Metadata m ModuleBundle
build (Module _ _ defs) = do
  forM_ defs build2
  undefined

build2 :: (Monad m) => Definition Metadata Kind () -> CompilerT Metadata m ModuleBundle
build2 =
  \case
    DType loc name d ->
      undefined
    DCotype loc name d ->
      undefined
    DFunction loc name fs ds ->
      undefined
    DConstant loc name d ds ->
      undefined
    DImport loc path ns ->
      undefined
    DTrait loc name t ->
      undefined
    DInstance loc name d ->
      undefined
    DTypeAlias loc name a ->
      undefined
    DFold loc name d ->
      undefined
    DUnfold loc name d ->
      undefined
