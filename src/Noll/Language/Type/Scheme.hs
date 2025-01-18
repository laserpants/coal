{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Type.Scheme (Scheme (..), forall0, forall1, forall2, forall3) where

import Data.Set (Set)
import Noll.Language.Trait (Trait (..))
import Noll.Language.Type (Type (..), TypeIndex (..))
import Noll.Language.Type.Kind (Kind (..))

import qualified Data.Set as Set

data Scheme o k t = Forall (Set (o k)) [Trait t] t
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

{-# INLINE index #-}
index :: Int -> TypeIndex Kind
index = TypeIndex KType

forall0 :: t -> Scheme TypeIndex Kind t
forall0 f = Forall mempty [] f

forall1 :: (Type TypeIndex Kind -> t) -> Scheme TypeIndex Kind t
forall1 f = Forall (Set.singleton a0) [] (f (TVariable a0))
 where
  a0 = index 0

forall2 :: (Type TypeIndex Kind -> Type TypeIndex Kind -> t) -> Scheme TypeIndex Kind t
forall2 f = Forall (Set.fromList [a0, a1]) [] (f (TVariable a0) (TVariable a1))
 where
  (a0, a1) = (index 0, index 1)

forall3 :: (Type TypeIndex Kind -> Type TypeIndex Kind -> Type TypeIndex Kind -> t) -> Scheme TypeIndex Kind t
forall3 f = Forall (Set.fromList [a0, a1, a2]) [] (f (TVariable a0) (TVariable a1) (TVariable a2))
 where
  (a0, a1, a2) = (index 0, index 1, index 2)

forall4 :: (Type TypeIndex Kind -> Type TypeIndex Kind -> Type TypeIndex Kind -> Type TypeIndex Kind -> t) -> Scheme TypeIndex Kind t
forall4 f = Forall (Set.fromList [a0, a1, a2, a3]) [] (f (TVariable a0) (TVariable a1) (TVariable a2) (TVariable a3))
 where
  (a0, a1, a2, a3) = (index 0, index 1, index 2, index 3)
