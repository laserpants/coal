{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Noll.Language.HasFree (
  HasBound (..),
  HasFree (..),
  --  appearsFreeIn,
  --  appearsIn,
) where

import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (universeBi)
import Data.Set (Set, singleton)
import Noll.Common.List1 (NonEmpty)
import Noll.Label (Label (..), labelName)
import Noll.Language.Expression (Clause (..), Expression (..))
import Noll.Language.Expression.Binding (Binding (..))
import Noll.Language.Expression.Choice (Choice (..), Guard (..))
import Noll.Language.Pattern (Pattern (..))
import Noll.Utils (Dictionary, Map, Name)

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
  boundIn (Label _ name) = singleton name

instance (Data a, Data t, Typeable e, Data (e a t)) => HasBound (Binding e a t) where
  boundIn = Set.fromList . universeBi

instance (Data a, Data t) => HasBound (Pattern a t) where
  boundIn = Set.fromList . universeBi

class HasFree f t | f -> t where
  freeIn :: f -> Set (Label t)

instance (Ord t, HasFree f t) => HasFree (Maybe f) t where
  freeIn = Set.unions . fmap freeIn

instance (Ord t, HasFree f t) => HasFree [f] t where
  freeIn = Set.unions . fmap freeIn

instance (Ord t, HasFree f t) => HasFree (NonEmpty f) t where
  freeIn = Set.unions . fmap freeIn

instance (Ord t, HasFree f t) => HasFree (Map a f) t where
  freeIn = Set.unions . fmap freeIn

instance (Ord t, Data a, Data t) => HasFree (Guard Expression a t) t where
  freeIn = Set.fromList . universeBi

instance (Ord t, Data a, Data t) => HasFree (Choice Expression a t) t where
  freeIn = Set.fromList . universeBi

instance (Ord t, Data a, Data t) => HasFree (Clause a t) t where
  freeIn =
    \case
      EClause _ p cs ->
        freeIn cs `exceptNames` boundIn p

instance (Ord t, Data a, Data t) => HasFree (Binding Expression a t) t where
  freeIn =
    \case
      BPattern _ _ e ->
        freeIn e
      BFunction _ _ ps e ->
        freeIn e `exceptNames` boundIn ps

instance (Ord t, Data t, Data a) => HasFree (Expression a t) t where
  freeIn =
    \case
      EConstructor{} ->
        mempty
      ELambda _ ps e ->
        freeIn e `exceptNames` boundIn ps
      ELet _ gs e1 ->
        freeIn gs <> (freeIn e1 `exceptNames` boundIn gs)
      ERecursiveLet _ p e1 e2 ->
        (freeIn e1 <> freeIn e2) `exceptNames` boundIn p
      EMatch _ _ e cs ->
        freeIn e <> freeIn cs
      ECompiledMatch{} ->
        error " TODO"
      e ->
        Set.fromList (universeBi e)

{-# INLINE exceptNames #-}
exceptNames :: (Foldable f) => Set (Label a) -> f Name -> Set (Label a)
exceptNames free bound = Set.filter (`notInNames` bound) free

{-# INLINE notInNames #-}
notInNames :: (Foldable f) => Label a -> f Name -> Bool
notInNames = notElem . labelName

-- {-# INLINE appearsFreeIn #-}
-- appearsFreeIn :: (HasFree a t) => Name -> a -> Bool
-- appearsFreeIn name = appearsIn name . freeIn

-- {-# INLINE appearsIn #-}
-- appearsIn :: Name -> Set (Label a) -> Bool
-- appearsIn name set = name `elem` Set.map labelName set
