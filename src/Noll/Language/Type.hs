{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Type (
  Type (..),
  TypeIndex (..),
  Parameter (..),
  HasActive (..),
  IndexedType,
  foldType,
  activeIdsIn,
  normalizeRowTypes,
  (~>),
) where

import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (transform)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Lang.Common.List1 (List1)
import Lang.Common.Supply (Supply (..))
import Lang.Utils (Map, Name, Set)
import Noll.Language.Type.Intrinsic (Intrinsic (..))
import Noll.Language.Type.Kind (Kind)
import Noll.Language.Type.Row (Row (..), normalizeRow)

import qualified Data.Set as Set

data Type o k
  = TApplication k (Type o k) (List1 (Type o k))
  | TArrow (Type o k) (Type o k)
  | TConstructor k Name
  | TIntrinsic (Intrinsic (Type o k))
  | TRow (Row o k (Type o k))
  | TVariable (o k)
  | TAlias Name [Type o k] (Type o k)
  deriving (Show, Eq, Ord, Read, Data, Typeable, Generic)

instance (Hashable k, Hashable (o k)) => Hashable (Type o k)

infixr 1 `TArrow`

(~>) :: Type o k -> Type o k -> Type o k
(~>) = TArrow

infixr 1 ~>

data TypeIndex k = TypeIndex
  { typeIndexKind :: k
  , typeIndexId :: Int
  }
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Data, Typeable, Generic)

instance (Hashable k) => Hashable (TypeIndex k)

data Parameter k = Parameter
  { parameterKind :: k
  , parameterName :: Name
  }
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Data, Typeable, Generic)

instance (Hashable k) => Hashable (Parameter k)

type IndexedType = Type TypeIndex Kind

instance Supply (TypeIndex k) where
  updateSupply f (TypeIndex k t) = TypeIndex k (f t)
  getSupply (TypeIndex _ t) = t

class HasActive k t | t -> k where
  activeIn :: t -> Set (TypeIndex k)

instance (HasActive Kind t) => HasActive Kind (Map a t) where
  activeIn = Set.unions . fmap activeIn

instance (HasActive Kind t) => HasActive Kind [t] where
  activeIn = Set.unions . fmap activeIn

instance (HasActive Kind t) => HasActive Kind (NonEmpty t) where
  activeIn = Set.unions . fmap activeIn

{-# INLINE activeIdsIn #-}
activeIdsIn :: (HasActive k t) => t -> Set Int
activeIdsIn = Set.map typeIndexId . activeIn

{-# INLINE foldType #-}
foldType :: (Foldable f) => Type o k -> f (Type o k) -> Type o k
foldType = foldr TArrow

normalizeRowTypes :: (Typeable o, Data k, Data (o k)) => Type o k -> Type o k
normalizeRowTypes = transform $
  \case
    TRow r ->
      TRow (normalizeRow r)
    t ->
      t
