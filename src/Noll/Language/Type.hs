{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Noll.Language.Type (
  Type (..),
  TypeIndex (..),
  TypeParam (..),
  HasActive (..),
  IndexedType,
  OpaqueType,
  foldType,
  activeIdsIn,
  normalizeRowTypes,
) where

import Data.List.NonEmpty (NonEmpty)
import Noll.Language.Type.Intrinsic (Intrinsic (..))
import Noll.Language.Type.Kind (Kind)
import Noll.Language.Type.Row (Row (..), normalizeRow)
import Noll.Lib.List1 (List1)
import Noll.Lib.Supply (Supply (..))
import Noll.Utils (Map, Name, Set)

import qualified Data.Set as Set

data Type o k
  = TApplication k (Type o k) (List1 (Type o k))
  | TArrow (Type o k) (Type o k)
  | TConstructor k Name
  | TIntrinsic (Intrinsic (Type o k))
  | TRow (Row o k (Type o k))
  | TVariable (o k)
  | TAlias Name [Type o k] (Type o k)
  deriving (Show, Eq, Ord, Read)

infixr 1 `TArrow`

data TypeIndex k = TypeIndex
  { typeIndexKind :: k
  , typeIndexId :: Int
  }
  deriving (Show, Eq, Ord, Read, Functor, Foldable)

data TypeParam k = TypeParam
  { typeParamKind :: k
  , typeParamName :: Name
  }
  deriving (Show, Eq, Ord, Read, Functor, Foldable)

type IndexedType = Type TypeIndex Kind

type OpaqueType = Type TypeIndex ()

instance Supply (TypeIndex k) where
  updateSupply f (TypeIndex k t) = TypeIndex k (f t)

class HasActive k t | t -> k where
  activeIn :: t -> Set (TypeIndex k)

instance (Ord k, HasActive k t) => HasActive k (Map a t) where
  activeIn = Set.unions . fmap activeIn

instance (Ord k, HasActive k t) => HasActive k [t] where
  activeIn = Set.unions . fmap activeIn

instance (Ord k, HasActive k t) => HasActive k (NonEmpty t) where
  activeIn = Set.unions . fmap activeIn

{-# INLINE activeIdsIn #-}
activeIdsIn :: (HasActive k t) => t -> Set Int
activeIdsIn t = Set.map typeIndexId (activeIn t)

{-# INLINE foldType #-}
foldType :: (Foldable f) => Type o k -> f (Type o k) -> Type o k
foldType = foldr TArrow

{-# INLINE normalizeRowTypes #-}
normalizeRowTypes :: Type o k -> Type o k
normalizeRowTypes =
  \case
    TRow r ->
      TRow (normalizeRow r)
    TApplication k t1 ts ->
      TApplication k (normalizeRowTypes t1) (fmap normalizeRowTypes ts)
    TArrow t1 t2 ->
      TArrow (normalizeRowTypes t1) (normalizeRowTypes t2)
    TIntrinsic t ->
      TIntrinsic (fmap normalizeRowTypes t)
    TAlias name ts t ->
      TAlias name (fmap normalizeRowTypes ts) (normalizeRowTypes t)
    t ->
      t
