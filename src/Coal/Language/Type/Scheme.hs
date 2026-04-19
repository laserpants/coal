{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Language.Type.Scheme

Polymorphic type schemes with universal quantification.
-}
module Coal.Language.Type.Scheme (
  Scheme (..),
  toScheme,
  forall0,
  forall1,
  forall1',
  forall2,
  forall2',
  forall3,
  forall3',
  forall4,
  forall4',
  forallN,
  forallN',
  IndexedScheme,
  listConstructorScheme,
  tupleScheme,
)
where

import Coal.Language.Trait (Trait (..))
import Coal.Language.Type (IndexedType, Parameter, ParameterizedType, Type (..), TypeIndex (..), (~>))
import Coal.Language.Type.Intrinsic (Intrinsic (..))
import Coal.Language.Type.Kind (Kind (..))
import Coal.Language.Type.Operations (listType, tupleType)
import Coal.Language.Type.Row (Row (..))
import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)

data Scheme o k t = Forall
  { schemeTypeVariables :: Set (o k)
  , schemeTraits :: Set (Trait t)
  , schemeTypeBody :: t
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Foldable
    , Data
    , Typeable
    , Generic
    )

instance (Binary (o k), Binary t) => Binary (Scheme o k t)

{-# INLINE index #-}
index :: Int -> TypeIndex Kind
index = TypeIndex KType

type IndexedScheme = Scheme TypeIndex Kind IndexedType

{-# INLINE forall0 #-}
forall0 :: IndexedType -> IndexedScheme
forall0 = Forall mempty mempty

forall1 :: (IndexedType -> IndexedType) -> IndexedScheme
forall1 f = Forall (Set.singleton a0) mempty (f (TVariable a0))
 where
  a0 = index 0

forall1' :: (IndexedType -> (Set (Trait IndexedType), IndexedType)) -> IndexedScheme
forall1' f = Forall (Set.singleton a0) traits t
 where
  (traits, t) = f (TVariable a0)
  a0 = index 0

forall2 :: (IndexedType -> IndexedType -> IndexedType) -> IndexedScheme
forall2 f = Forall (Set.fromList [a0, a1]) mempty (f (TVariable a0) (TVariable a1))
 where
  (a0, a1) = (index 0, index 1)

forall2' :: (IndexedType -> IndexedType -> (Set (Trait IndexedType), IndexedType)) -> IndexedScheme
forall2' f = Forall (Set.singleton a0) traits t
 where
  (traits, t) = f (TVariable a0) (TVariable a1)
  (a0, a1) = (index 0, index 1)

forall3 :: (IndexedType -> IndexedType -> IndexedType -> IndexedType) -> IndexedScheme
forall3 f = Forall (Set.fromList [a0, a1, a2]) mempty (f (TVariable a0) (TVariable a1) (TVariable a2))
 where
  (a0, a1, a2) = (index 0, index 1, index 2)

forall3' :: (IndexedType -> IndexedType -> IndexedType -> (Set (Trait IndexedType), IndexedType)) -> IndexedScheme
forall3' f = Forall (Set.fromList [a0, a1, a2]) traits t
 where
  (traits, t) = f (TVariable a0) (TVariable a1) (TVariable a2)
  (a0, a1, a2) = (index 0, index 1, index 2)

forall4 :: (IndexedType -> IndexedType -> IndexedType -> IndexedType -> IndexedType) -> IndexedScheme
forall4 f = Forall (Set.fromList [a0, a1, a2, a3]) mempty (f (TVariable a0) (TVariable a1) (TVariable a2) (TVariable a3))
 where
  (a0, a1, a2, a3) = (index 0, index 1, index 2, index 3)

forall4' :: (IndexedType -> IndexedType -> IndexedType -> IndexedType -> (Set (Trait IndexedType), IndexedType)) -> IndexedScheme
forall4' f = Forall (Set.fromList [a0, a1, a2, a3]) traits t
 where
  (traits, t) = f (TVariable a0) (TVariable a1) (TVariable a2) (TVariable a3)
  (a0, a1, a2, a3) = (index 0, index 1, index 2, index 3)

forallN :: Int -> ([IndexedType] -> IndexedType) -> IndexedScheme
forallN n f = Forall (Set.fromList ixs) mempty (f (TVariable <$> ixs))
 where
  ixs = [index i | i <- [0 .. n - 1]]

forallN' :: Int -> ([IndexedType] -> (Set (Trait IndexedType), IndexedType)) -> IndexedScheme
forallN' n f = Forall (Set.fromList ixs) traits t
 where
  (traits, t) = f (TVariable <$> ixs)
  ixs = [index i | i <- [0 .. n - 1]]

listConstructorScheme :: IndexedScheme
listConstructorScheme = forall1 (\a -> a ~> listType a ~> listType a)

tupleScheme :: Int -> IndexedScheme
tupleScheme n | n < 2 = error "Invalid tuple size"
tupleScheme n = forallN n (tupleType . NonEmpty.fromList)

-- | Helper to convert a type to a scheme by collecting its parameters
toScheme :: Type Parameter () -> Scheme Parameter () ParameterizedType
toScheme t = Forall (Set.fromList (params t)) mempty t

-- | Typeclass for extracting type parameters from types and related structures
class Parameterized p where
  params :: p -> [Parameter ()]

instance (Parameterized p) => Parameterized [p] where
  params = concatMap params

instance (Parameterized p) => Parameterized (NonEmpty p) where
  params = concatMap params

instance Parameterized (Type Parameter ()) where
  params =
    \case
      TVariable p ->
        params p
      TApplication _ t ts ->
        params t <> params ts
      TArrow t1 t2 ->
        params t1 <> params t2
      TIntrinsic t ->
        params t
      TRecord t ->
        params t
      TRow r ->
        params r
      TAlias _ _ t ->
        params t
      TConstructor{} ->
        []

instance Parameterized Intrinsic where
  params _ = []

instance Parameterized (Row Parameter () (Type Parameter ())) where
  params =
    \case
      RVariable p ->
        params p
      RExtend _ t r ->
        params t <> params r
      RNil ->
        []

instance Parameterized (Parameter ()) where
  params = return
