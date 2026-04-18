{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Language.Type

Core type representation for the Coal language type system.

Defines indexed and parameterized type variants including applications,
arrows, constructors, records, and row polymorphism.
-}
module Coal.Language.Type (
  Type (..),
  TypeIndex (..),
  Parameter (..),
  KindProxy (..),
  IndexedType,
  ParameterizedType,
  foldType,
  applyTypeArgs,
  (~>),
)
where

import Coal.Common.Supply (Supply (..))
import Coal.Language.Type.Intrinsic (Intrinsic (..))
import Coal.Language.Type.Kind (Kind (..), KindProxy (..))
import Coal.Language.Type.Row (Row (..))
import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Extras (Name)
import GHC.Generics (Generic)

data Type o k
  = TApplication k (Type o k) (Type o k)
  | TArrow (Type o k) (Type o k)
  | TConstructor k Name
  | TIntrinsic Intrinsic
  | TRecord (Type o k)
  | TRow (Row o k (Type o k))
  | TVariable (o k)
  | TAlias Name [Type o k] (Type o k)
  deriving (Show, Eq, Ord, Read, Data, Typeable, Generic)

instance (Binary (o k), Binary k) => Binary (Type o k)

infixr 1 `TArrow`

(~>) :: Type o k -> Type o k -> Type o k
(~>) = TArrow

infixr 1 ~>

data TypeIndex k = TypeIndex
  { typeIndexKind :: k
  , typeIndexId :: Int
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Data
    , Typeable
    , Generic
    )

instance (Binary k) => Binary (TypeIndex k)

instance Supply (TypeIndex k) where
  updateSupply f (TypeIndex k t) = TypeIndex k (f t)
  getSupply (TypeIndex _ t) = t

type IndexedType = Type TypeIndex Kind

data Parameter k = Parameter
  { parameterKind :: k
  , parameterName :: Name
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Data
    , Typeable
    , Generic
    )

instance (Binary k) => Binary (Parameter k)

type ParameterizedType = Type Parameter ()

{-# INLINE foldType #-}
foldType :: (Foldable f) => Type o k -> f (Type o k) -> Type o k
foldType = foldr TArrow

applyTypeArgs :: (KindProxy (Type o k) k) => k -> Type o k -> NonEmpty (Type o k) -> Type o k
applyTypeArgs k ty = go ty . NonEmpty.toList
 where
  go t =
    \case
      [t1] ->
        TApplication k t t1
      t1 : ts ->
        go (TApplication (tailKind t) t t1) ts
      _ ->
        error "Implementation error"
