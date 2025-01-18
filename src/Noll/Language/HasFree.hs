{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.HasFree (
  HasBound (..),
  HasFree (..),
  isBoundIn,
  isNotBoundIn,
) where

import Data.Set (Set)
import Noll.Common.List1 (NonEmpty)
import Noll.Label (Label (..))
import Noll.Language.Expression (Clause (..), Expression (..))
import Noll.Language.Expression.Binding (Binding (..))
import Noll.Language.Expression.Choice (Choice (..), Guard (..))
import Noll.Language.Pattern (Pattern (..))
import Noll.Utils (Dictionary (..), Map, Name)

import qualified Data.Set as Set

class HasBound b where
  boundIn :: b -> Set Name

instance (HasBound b) => HasBound (Dictionary b) where
  boundIn = Set.unions . fmap boundIn

instance (HasBound b) => HasBound (Maybe b) where
  boundIn = Set.unions . fmap boundIn

instance (HasBound b) => HasBound [b] where
  boundIn = Set.unions . fmap boundIn

instance (HasBound b) => HasBound (NonEmpty b) where
  boundIn = Set.unions . fmap boundIn

instance HasBound (Label t) where
  boundIn (Label _ name) = Set.singleton name

instance (Ord t) => HasBound (Binding e a t) where
  boundIn =
    \case
      BPattern _ p _ ->
        boundIn p
      BFunction _ _ ps _ ->
        boundIn ps

instance (Ord t) => HasBound (Pattern a t) where
  boundIn =
    \case
      PVariable _ (Label _ name) ->
        Set.singleton name

class HasFree f t where
  freeIn :: f -> Set (Label t)

instance (Ord t, HasFree f t) => HasFree (Maybe f) t where
  freeIn = Set.unions . fmap freeIn

instance (Ord t, HasFree f t) => HasFree [f] t where
  freeIn = Set.unions . fmap freeIn

instance (Ord t, HasFree f t) => HasFree (NonEmpty f) t where
  freeIn = Set.unions . fmap freeIn

instance (Ord t, HasFree f t) => HasFree (Map a f) t where
  freeIn = Set.unions . fmap freeIn

instance (Ord t) => HasFree (Guard Expression a t) t where
  freeIn =
    \case
      CGuard e ->
        freeIn e

instance (Ord t) => HasFree (Clause Expression a t) t where
  freeIn =
    undefined

instance (Ord t) => HasFree (Binding Expression a t) t where
  freeIn =
    undefined

instance (Ord t) => HasFree (Expression a t) t where
  freeIn =
    undefined

{-# INLINE isBoundIn #-}
isBoundIn :: (HasBound b) => Name -> b -> Bool
isBoundIn name obj = name `elem` boundIn obj

{-# INLINE isNotBoundIn #-}
isNotBoundIn :: (HasBound b) => Name -> b -> Bool
isNotBoundIn name obj = name `notElem` boundIn obj
