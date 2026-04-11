-- +
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

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
  { protoOtypeDefinitionParameters :: [Parameter k]
  , protoOtypeDefinitionConstructors :: [DataConstructor Parameter k (Type Parameter k)]
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
  { protoOfunctionDefinitionMetadata :: a
  , protoOfunctionDefinitionAnnotation :: Maybe (Qualified (Type Parameter k))
  , protoOfunctionDefinitionType :: Qualified t
  , protoOfunctionDefinitionPatterns :: NonEmpty (Pattern a k t)
  , protoOfunctionDefinitionExpression :: Expression a k t
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
  { protoOletDefinitionMetadata :: a
  , protoOletDefinitionAnnotation :: Maybe (Qualified (Type Parameter k))
  , protoOletDefinitionType :: Qualified t
  , protoOletDefinitionExpression :: Expression a k t
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
  { protoOfoldDefinitionMetadata :: a
  , protoOfoldDefinitionAnnotation :: Maybe (Qualified (Type Parameter k))
  , protoOfoldDefinitionClauses :: NonEmpty (Clause a k t)
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
  { protoOtraitDefinitionMetadata :: a
  , protoOtraitDefinitionTraitName :: Name
  , protoOtraitDefinitionConstraints :: [Trait (Parameter k)]
  , protoOtraitDefinitionParameter :: Parameter k
  , protoOtraitDefinitionInterface :: [TraitDefinitionInterfaceEntry k]
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
  { protoOtraitDefinitionInterfaceEntryName :: Name
  , protoOtraitDefinitionInterfaceEntryScheme :: Scheme Parameter k (Type Parameter k)
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
  { protoOinstanceDefinitionMetadata :: a
  , protoOinstanceDefinitionTraitName :: Name
  , protoOinstanceDefinitionConstraints :: [Trait (Parameter k)]
  , protoOinstanceDefinitionType :: Type Parameter k
  , protoOinstanceDefinitionImplementations :: [Definition a k t]
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
  Trait protoOinstanceDefinitionTraitName protoOinstanceDefinitionType

data AliasDefinition a k = AliasDefinition
  { protoOaliasDefinitionParameters :: [Parameter k]
  , protoOaliasDefinitionType :: Type Parameter k
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
