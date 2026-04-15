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

module Coal.Language.Type (
  Type (..),
  TypeIndex (..),
  Parameter (..),
  KindProxy (..),
  IndexedType,
  ParameterizedType,
  foldType,
  unfoldType,
  rowNormalize,
  applyTypeArgs,
  typeArgs,
  listType,
  tupleType,
  tupleTypeConstructor,
  isTupleType,
  fieldsRecordType,
  recordType,
  headConstructor,
  constructors,
  (~>),
)
where

import Coal.Common.Supply (Supply (..))
import Coal.Language.Type.Intrinsic (Intrinsic (..))
import Coal.Language.Type.Kind (Kind (..), KindProxy (..), tupleKind)
import Coal.Language.Type.Row (Row (..), fromDictionary, normalizeRow)
import Coal.Utils (intToVar)
import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (transform)
import Data.List.NonEmpty (NonEmpty (..), toList, (<|))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Data.Text (isPrefixOf)
import Extras (Dictionary, Name, Set)
import GHC.Generics (Generic)
import TextShow (showt)

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

unfoldType :: Type o k -> NonEmpty (Type o k)
unfoldType =
  \case
    TArrow t1 t2 ->
      t1 <| unfoldType t2
    t ->
      NonEmpty.singleton t

rowNormalize :: (Typeable o, Data k, Data (o k)) => Type o k -> Type o k
rowNormalize = transform $
  \case
    TRow r ->
      TRow (normalizeRow r)
    t ->
      t

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

typeArgs :: Type o k -> (Type o k, NonEmpty (Type o k))
typeArgs (TApplication _ t1 t2) = (t, NonEmpty.prependList ts (NonEmpty.singleton t2))
 where
  (t, ts) = go t1
  go =
    \case
      TApplication _ u1 u2 ->
        let (u, us) = go u1 in (u, us <> [u2])
      u ->
        (u, [])
typeArgs _ = error "Implementation error"

headConstructor :: Type o k -> Maybe Name
headConstructor =
  \case
    TApplication _ t _ ->
      headConstructor t
    TConstructor _ name ->
      Just name
    _ ->
      Nothing

listType :: IndexedType -> IndexedType
listType t = applyTypeArgs KType (TConstructor (KArrow KType KType) "List") (t :| [])

tupleType :: NonEmpty IndexedType -> IndexedType
tupleType ts = applyTypeArgs KType (TConstructor (tupleKind n) (tupleTypeConstructor n)) ts
 where
  n = length ts

isTupleType :: Type o k -> Bool
isTupleType t =
  case headConstructor t of
    Just con
      | "#Tuple" `isPrefixOf` con ->
          True
    _ ->
      False

{-# INLINE tupleTypeConstructor #-}
tupleTypeConstructor :: Int -> Name
tupleTypeConstructor n = "#Tuple" <> showt n

{-# INLINE recordType #-}
recordType :: Row o k (Type o k) -> Type o k
recordType = TRecord . TRow

fieldsRecordType :: Dictionary (Type o k) -> Row o k (Type o k) -> Type o k
fieldsRecordType fields row = recordType (fromDictionary fields row)

constructors :: Type o k -> Set Name
constructors =
  \case
    TApplication _ t1 t2 ->
      constructors t1 <> constructors t2
    TArrow t1 t2 ->
      constructors t1 <> constructors t2
    TConstructor _ name ->
      Set.singleton name
    TIntrinsic{} ->
      mempty
    TRecord r ->
      constructors r
    TRow r ->
      rowConstructors r
    TVariable{} ->
      mempty
    TAlias name _ t ->
      Set.insert name (constructors t)

rowConstructors :: Row o k (Type o k) -> Set Name
rowConstructors =
  \case
    RExtend _ t r ->
      constructors t <> rowConstructors r
    RVariable{} ->
      mempty
    RNil ->
      mempty
