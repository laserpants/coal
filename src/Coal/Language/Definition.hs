{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Language.Definition

Internal representation of module-level declarations in Coal source programs.

A Definition represents any construct that can appear at the top level of a
module, including data types, type aliases, functions, folds, imports, traits,
and trait instances.
-}
module Coal.Language.Definition (
  Definition (..),
  TypeDefinition (..),
  FunctionDefinition (..),
  FoldDefinition (..),
  LetDefinition (..),
  TraitDefinition (..),
  InstanceDefinition (..),
  AliasDefinition (..),
  TraitDefinitionInterfaceEntry (..),
  instanceDefinitionTrait,
) where

import Coal.Language.Data.Constructor (DataConstructor (..))
import Coal.Language.Expression
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (..))
import Coal.Language.Pattern
import Coal.Language.Trait
import Coal.Language.Type
import Coal.Language.Type.Kind
import Coal.Language.Type.Scheme
import Data.Binary
import Data.Data (Data, Typeable)
import Data.List.NonEmpty (NonEmpty)
import Extras (Name)
import GHC.Generics (Generic)

data TypeDefinition a k t = TypeDefinition
  { typeDefinitionParameters :: [Parameter k]
  , typeDefinitionConstructors :: [DataConstructor Parameter k (Type Parameter k)]
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Traversable
    , Data
    , Typeable
    , Generic
    )

instance (Binary a, Binary k, Binary t) => Binary (TypeDefinition a k t)

data FunctionDefinition a k t = FunctionDefinition
  { functionDefinitionMetadata :: a
  , functionDefinitionAnnotation :: Maybe (Qualified (Type Parameter k))
  , functionDefinitionType :: Qualified t
  , functionDefinitionPatterns :: NonEmpty (Pattern a k t)
  , functionDefinitionExpression :: Expression a k t
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Traversable
    , Data
    , Typeable
    , Generic
    )

instance (Binary a, Binary k, Binary t) => Binary (FunctionDefinition a k t)

data LetDefinition a k t = LetDefinition
  { letDefinitionMetadata :: a
  , letDefinitionAnnotation :: Maybe (Qualified (Type Parameter k))
  , letDefinitionType :: Qualified t
  , letDefinitionExpression :: Expression a k t
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Traversable
    , Data
    , Typeable
    , Generic
    )

instance (Binary a, Binary k, Binary t) => Binary (LetDefinition a k t)

data FoldDefinition a k t = FoldDefinition
  { foldDefinitionMetadata :: a
  , foldDefinitionAnnotation :: Maybe (Qualified (Type Parameter k))
  , foldDefinitionClauses :: NonEmpty (Clause a k t)
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Traversable
    , Data
    , Typeable
    , Generic
    )

instance (Binary a, Binary k, Binary t) => Binary (FoldDefinition a k t)

data TraitDefinition a k = TraitDefinition
  { traitDefinitionMetadata :: a
  , traitDefinitionTraitName :: Name
  , traitDefinitionConstraints :: [Trait (Parameter k)]
  , traitDefinitionParameter :: Parameter k
  , traitDefinitionInterface :: [TraitDefinitionInterfaceEntry k]
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Data
    , Typeable
    , Generic
    )

instance (Binary a, Binary k) => Binary (TraitDefinition a k)

data TraitDefinitionInterfaceEntry k = TraitDefinitionInterfaceEntry
  { traitDefinitionInterfaceEntryName :: Name
  , traitDefinitionInterfaceEntryScheme :: Scheme Parameter k (Type Parameter k)
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Data
    , Typeable
    , Generic
    )

instance (Binary k) => Binary (TraitDefinitionInterfaceEntry k)

data InstanceDefinition a k t = InstanceDefinition
  { instanceDefinitionMetadata :: a
  , instanceDefinitionTraitName :: Name
  , instanceDefinitionConstraints :: [Trait (Parameter k)]
  , instanceDefinitionType :: Type Parameter k
  , instanceDefinitionImplementations :: [Definition a k t]
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Traversable
    , Data
    , Typeable
    , Generic
    )

instance (Binary a, Binary k, Binary t) => Binary (InstanceDefinition a k t)

instanceDefinitionTrait :: InstanceDefinition a Kind t -> Trait (Type Parameter Kind)
instanceDefinitionTrait InstanceDefinition{..} =
  Trait instanceDefinitionTraitName instanceDefinitionType

data AliasDefinition a k = AliasDefinition
  { aliasDefinitionParameters :: [Parameter k]
  , aliasDefinitionType :: Type Parameter k
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Data
    , Typeable
    , Generic
    )

instance (Binary a, Binary k) => Binary (AliasDefinition a k)

data Definition a k t
  = -- | Type definition
    DType a Name (TypeDefinition a k t)
  | -- | Type alias
    DTypeAlias a Name (AliasDefinition a k)
  | -- | Function definition
    DFunction a Name (FunctionDefinition a k t)
  | -- | Function
    DFunctionGroup a Name [FunctionDefinition a k t]
  | -- | Top-level fold
    DFold a Name (FoldDefinition a k t)
  | -- | Top-level let-binding
    DLet a Name (LetDefinition a k t)
  | -- | Import statement
    DImport a Path [Import a]
  | -- | Namespace (qualified) import
    DNamespaceImport a Path
  | -- | Trait
    DTrait a Name (TraitDefinition a k)
  | -- | Trait instance
    DInstance a (InstanceDefinition a k t)
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Traversable
    , Data
    , Typeable
    , Generic
    )

instance (Binary a, Binary k, Binary t) => Binary (Definition a k t)
