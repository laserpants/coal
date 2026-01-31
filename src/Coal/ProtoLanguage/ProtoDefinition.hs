{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoLanguage.ProtoDefinition (
  ProtoDefinition (..),
  ProtoTypeDefinition (..),
  ProtoFunctionDefinition (..),
  ProtoFoldDefinition (..),
  ProtoLetDefinition (..),
  ProtoTraitDefinition (..),
  ProtoInstanceDefinition (..),
  ProtoAliasDefinition (..),
  instanceDefinitionTrait,
) where

import Coal.Language
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (..))
import Data.Data (Data, Typeable)
import Data.List.NonEmpty (NonEmpty)
import Extras (Name)

data ProtoTypeDefinition a k t = ProtoTypeDefinition
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
    )

data ProtoFunctionDefinition a k t = ProtoFunctionDefinition
  { protoOfunctionDefinitionMetadata :: a
  , protoOfunctionDefinitionAnnotation :: Maybe (With (Type Parameter k))
  , protoOfunctionDefinitionType :: With t
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
    )

data ProtoLetDefinition a k t = ProtoLetDefinition
  { protoOletDefinitionMetadata :: a
  , protoOletDefinitionAnnotation :: Maybe (With (Type Parameter k))
  , protoOletDefinitionType :: With t
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
    )

data ProtoFoldDefinition a k t = ProtoFoldDefinition
  { protoOfoldDefinitionMetadata :: a
  , protoOfoldDefinitionAnnotation :: Maybe (With (Type Parameter k))
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
    )

data ProtoTraitDefinition a k = ProtoTraitDefinition
  { protoOtraitDefinitionMetadata :: a
  , protoOtraitDefinitionTraitName :: Name
  , protoOtraitDefinitionConstraints :: [Trait (Parameter k)]
  , protoOtraitDefinitionParameter :: Parameter k
  , protoOtraitDefinitionInterface :: [(Name, Scheme Parameter k (Type Parameter k))]
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Data
    , Typeable
    )

data ProtoInstanceDefinition a k t = ProtoInstanceDefinition
  { protoOinstanceDefinitionMetadata :: a
  , protoOinstanceDefinitionTraitName :: Name
  , protoOinstanceDefinitionConstraints :: [Trait (Parameter k)]
  , protoOinstanceDefinitionType :: Type Parameter k
  , protoOinstanceDefinitionImplementations :: [ProtoDefinition a k t]
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
    )

instanceDefinitionTrait :: ProtoInstanceDefinition a Kind t -> Trait (Type Parameter Kind)
instanceDefinitionTrait ProtoInstanceDefinition{..} =
  Trait protoOinstanceDefinitionTraitName protoOinstanceDefinitionType

data ProtoAliasDefinition a k = ProtoAliasDefinition
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
    )

data ProtoDefinition a k t
  = -- | Type definition
    ProtoDType a Name (ProtoTypeDefinition a k t)
  | -- | Type alias
    ProtoDTypeAlias a Name (ProtoAliasDefinition a k)
  | -- | Function definition
    ProtoDFunction a Name (ProtoFunctionDefinition a k t)
  | -- | Function
    ProtoDFunctionGroup a Name [ProtoFunctionDefinition a k t]
  | -- | Top-level fold
    ProtoDFold a Name (ProtoFoldDefinition a k t)
  | -- | Top-level let-binding
    ProtoDLet a Name (ProtoLetDefinition a k t)
  | -- | Import statement
    ProtoDImport a Path [Import a]
  | -- | Namespace (qualified) import
    ProtoDQualifiedImport a Path
  | -- | Trait
    ProtoDTrait a Name (ProtoTraitDefinition a k)
  | -- | Trait instance
    ProtoDInstance a (ProtoInstanceDefinition a k t)
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
    )
