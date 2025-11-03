module Coal.Compiler.Module.Bundle where

import Coal.Common.Environment (Environment (..))
import Coal.Language
import Data.Map.Strict (Map)
import Extras (Dictionary, Name, Set)

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

data ModuleBundle = ModuleBundle
  { moduleDataConstructors :: Environment DataConstructorInfo
  , moduleCodataAccessors :: Environment (CodataAccessor TypeIndex Kind IndexedType)
  , moduleTypeConstructors :: Environment Kind
  , moduleTraits :: Environment TraitInfo
  , moduleInstances :: Environment InstanceInfo
  , moduleAliases :: Environment AliasInfo
  }
  deriving (Show, Eq, Ord, Read)
