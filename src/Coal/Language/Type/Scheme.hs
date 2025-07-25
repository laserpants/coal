{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Type.Scheme (
  Scheme (..),
  forall0,
  forall1,
  forall2,
  forall3,
  forall4,
  IndexedScheme,
) where

import Data.Data (Data, Typeable)
import Data.Set (Set)
import GHC.Generics (Generic)
import Coal.Language.Trait (Trait (..))
import Coal.Language.Type (IndexedType, Type (..), TypeIndex (..))
import Coal.Language.Type.Kind (Kind (..))

import qualified Data.Set as Set

data Scheme o k t = Forall (Set (o k)) [Trait t] t
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable, Generic)

{-# INLINE index #-}
index :: Int -> TypeIndex Kind
index = TypeIndex KType

{-# INLINE forall0 #-}
forall0 :: t -> Scheme TypeIndex Kind t
forall0 = Forall mempty []

type IndexedScheme = Scheme TypeIndex Kind IndexedType

forall1 :: (IndexedType -> IndexedType) -> IndexedScheme
forall1 f = Forall (Set.singleton a0) [] (f (TVariable a0))
 where
  a0 = index 0

forall2 :: (IndexedType -> IndexedType -> IndexedType) -> IndexedScheme
forall2 f = Forall (Set.fromList [a0, a1]) [] (f (TVariable a0) (TVariable a1))
 where
  (a0, a1) = (index 0, index 1)

forall3 :: (IndexedType -> IndexedType -> IndexedType -> IndexedType) -> IndexedScheme
forall3 f = Forall (Set.fromList [a0, a1, a2]) [] (f (TVariable a0) (TVariable a1) (TVariable a2))
 where
  (a0, a1, a2) = (index 0, index 1, index 2)

forall4 :: (IndexedType -> IndexedType -> IndexedType -> IndexedType -> IndexedType) -> IndexedScheme
forall4 f = Forall (Set.fromList [a0, a1, a2, a3]) [] (f (TVariable a0) (TVariable a1) (TVariable a2) (TVariable a3))
 where
  (a0, a1, a2, a3) = (index 0, index 1, index 2, index 3)
