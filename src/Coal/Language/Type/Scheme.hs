{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Type.Scheme (
  Scheme (..),
  bound,
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
) where

import Coal.Language.Trait (Trait (..))
import Coal.Language.Type (IndexedType, Type (..), TypeIndex (..), listType, tupleType, (~>))
import Coal.Language.Type.Kind (Kind (..))
import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import Data.List (intersperse)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), hsep)

data Scheme o k t = Forall (Set (o k)) [Trait t] t
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

instance (Binary (o k), Binary t) => Binary (Scheme o k t)

{-# INLINE bound #-}
bound :: Scheme o k t -> Set (o k)
bound (Forall s _ _) = s

instance (Pretty k, Pretty (o k), Pretty t) => Pretty (Scheme o k t) where
  pretty =
    \case
      Forall _ ts t ->
        pretty t <> traits
       where
        traits =
          case ts of
            [] -> ""
            _ -> " with " <> hsep (intersperse "," (pretty <$> ts))

{-# INLINE index #-}
index :: Int -> TypeIndex Kind
index = TypeIndex KType

type IndexedScheme = Scheme TypeIndex Kind IndexedType

{-# INLINE forall0 #-}
forall0 :: IndexedType -> IndexedScheme
forall0 = Forall mempty []

forall1 :: (IndexedType -> IndexedType) -> IndexedScheme
forall1 f = Forall (Set.singleton a0) [] (f (TVariable a0))
 where
  a0 = index 0

forall1' :: (IndexedType -> ([Trait IndexedType], IndexedType)) -> IndexedScheme
forall1' f = Forall (Set.singleton a0) traits t
 where
  (traits, t) = f (TVariable a0)
  a0 = index 0

forall2 :: (IndexedType -> IndexedType -> IndexedType) -> IndexedScheme
forall2 f = Forall (Set.fromList [a0, a1]) [] (f (TVariable a0) (TVariable a1))
 where
  (a0, a1) = (index 0, index 1)

forall2' :: (IndexedType -> IndexedType -> ([Trait IndexedType], IndexedType)) -> IndexedScheme
forall2' f = Forall (Set.singleton a0) traits t
 where
  (traits, t) = f (TVariable a0) (TVariable a1)
  (a0, a1) = (index 0, index 1)

forall3 :: (IndexedType -> IndexedType -> IndexedType -> IndexedType) -> IndexedScheme
forall3 f = Forall (Set.fromList [a0, a1, a2]) [] (f (TVariable a0) (TVariable a1) (TVariable a2))
 where
  (a0, a1, a2) = (index 0, index 1, index 2)

forall3' :: (IndexedType -> IndexedType -> IndexedType -> ([Trait IndexedType], IndexedType)) -> IndexedScheme
forall3' f = Forall (Set.fromList [a0, a1, a2]) traits t
 where
  (traits, t) = f (TVariable a0) (TVariable a1) (TVariable a2)
  (a0, a1, a2) = (index 0, index 1, index 2)

forall4 :: (IndexedType -> IndexedType -> IndexedType -> IndexedType -> IndexedType) -> IndexedScheme
forall4 f = Forall (Set.fromList [a0, a1, a2, a3]) [] (f (TVariable a0) (TVariable a1) (TVariable a2) (TVariable a3))
 where
  (a0, a1, a2, a3) = (index 0, index 1, index 2, index 3)

forall4' :: (IndexedType -> IndexedType -> IndexedType -> IndexedType -> ([Trait IndexedType], IndexedType)) -> IndexedScheme
forall4' f = Forall (Set.fromList [a0, a1, a2, a3]) traits t
 where
  (traits, t) = f (TVariable a0) (TVariable a1) (TVariable a2) (TVariable a3)
  (a0, a1, a2, a3) = (index 0, index 1, index 2, index 3)

forallN :: Int -> ([IndexedType] -> IndexedType) -> IndexedScheme
forallN n f = Forall (Set.fromList ixs) [] (f (TVariable <$> ixs))
 where
  ixs = [index i | i <- [0 .. n - 1]]

forallN' :: Int -> ([IndexedType] -> ([Trait IndexedType], IndexedType)) -> IndexedScheme
forallN' n f = Forall (Set.fromList ixs) traits t
 where
  (traits, t) = f (TVariable <$> ixs)
  ixs = [index i | i <- [0 .. n - 1]]

listConstructorScheme :: IndexedScheme
listConstructorScheme = forall1 (\a -> a ~> listType a ~> listType a)

tupleScheme :: Int -> IndexedScheme
tupleScheme n | n < 2 = error "Invalid tuple size"
tupleScheme n = forallN n (tupleType . NonEmpty.fromList)
