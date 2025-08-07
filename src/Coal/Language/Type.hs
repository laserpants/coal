{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Type (
  Type (..),
  TypeIndex (..),
  Parameter (..),
  HasActive (..),
  IndexedType,
  foldType,
  unfoldType,
  activeIdsIn,
  normalizeRowTypes,
  listType,
  (~>),
) where

import Coal.Common.List1 (List1, NonEmpty (..), (<|))
import Coal.Common.Supply (Supply (..))
import Coal.Language.Type.Intrinsic (Intrinsic (..))
import Coal.Language.Type.Kind (Kind (..))
import Coal.Language.Type.Row (Row (..), normalizeRow)
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (transform)
import Extra (Map, Name, Set)
import GHC.Generics (Generic)

import qualified Coal.Common.List1 as List1
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

infixr 1 `TArrow`

(~>) :: Type o k -> Type o k -> Type o k
(~>) = TArrow

infixr 1 ~>

data TypeIndex k = TypeIndex
  { typeIndexKind :: k
  , typeIndexId :: Int
  }
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Data, Typeable, Generic)

data Parameter k = Parameter
  { parameterKind :: k
  , parameterName :: Name
  }
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Data, Typeable, Generic)

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

unfoldType :: Type o k -> List1 (Type o k)
unfoldType =
  \case
    TArrow t1 t2 ->
      t1 <| unfoldType t2
    t ->
      List1.singleton t

normalizeRowTypes :: (Typeable o, Data k, Data (o k)) => Type o k -> Type o k
normalizeRowTypes = transform $
  \case
    TRow r ->
      TRow (normalizeRow r)
    t ->
      t

listType :: IndexedType -> IndexedType
listType t = TApplication KType (TConstructor (KArrow KType KType) "List") (t :| [])
