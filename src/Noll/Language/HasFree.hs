{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Noll.Language.HasFree (
  HasBound (..),
  HasFree (..),
  isBoundIn,
  isNotBoundIn,
  appearsFreeIn,
  appearsIn,
) where

import Data.Set (Set)
import Noll.Common.List1 (NonEmpty)
import Noll.Label (Label (..), labelName)
import Noll.Language.Expression (Clause (..), Expression (..))
import Noll.Language.Expression.Binding (Binding (..))
import Noll.Language.Expression.Choice (Choice (..), Guard (..))
import Noll.Language.Pattern (Pattern (..))
import Noll.Utils (Dictionary (..), Map, Name, unionMap, (<$$>))

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
      PShorthand _ (Label _ name) ->
        Set.singleton name
      PAtVariable _ (Label _ name) ->
        Set.singleton name
      PAnnotation _ _ p ->
        boundIn p
      PConstructor _ _ ps ->
        boundIn ps
      PRecord a _ d p ->
        boundIn d <> boundIn p
      PListCons _ _ p1 p2 ->
        boundIn p1 <> boundIn p2
      PListLiteral _ _ ps ->
        boundIn ps
      POr _ _ p1 p2 ->
        boundIn p1 <> boundIn p2
      PAny{} ->
        mempty
      PLiteral{} ->
        mempty

class HasFree f t | f -> t where
  freeIn :: f -> Set (Label t)

instance (Ord t, HasFree f t) => HasFree (Maybe f) t where
  freeIn = undefined -- Set.unions . fmap freeIn

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

instance (Ord t) => HasFree (Choice Expression a t) t where
  freeIn =
    \case
      CPlain _ gs e ->
        freeIn gs <> freeIn e
      CLambda{} ->
        error "TODO"

instance (Ord t) => HasFree (Clause Expression a t) t where
  freeIn =
    \case
      EClause _ p cs ->
        freeIn cs `exceptNames` boundIn p

instance (Ord t) => HasFree (Binding Expression a t) t where
  freeIn =
    \case
      BPattern _ _ e ->
        freeIn e
      BFunction _ _ ps e ->
        freeIn e `exceptNames` boundIn ps

instance (Ord t) => HasFree (Expression a t) t where
  freeIn =
    \case
      EVariable _ ll ->
        Set.singleton ll
      EIf _ _ e1 e2 e3 ->
        freeIn e1 <> freeIn e2 <> freeIn e3
      ELambda _ ps e ->
        freeIn e `exceptNames` boundIn ps
      EApplication _ _ e es ->
        freeIn e <> freeIn es
      ELet _ gs e1 ->
        freeIn gs <> (freeIn e1 `exceptNames` boundIn gs)
      ELiteral{} ->
        mempty
      EConstructor{} ->
        mempty
      EMatch _ _ e cs ->
        freeIn e <> freeIn cs

{-# INLINE exceptNames #-}
exceptNames :: (Foldable f) => Set (Label a) -> f Name -> Set (Label a)
exceptNames free bound = Set.filter (`notInNames` bound) free

{-# INLINE notInNames #-}
notInNames :: (Foldable f) => Label b -> f Name -> Bool
notInNames = notElem . labelName

{-# INLINE isBoundIn #-}
isBoundIn :: (HasBound b) => Name -> b -> Bool
isBoundIn name obj = name `elem` boundIn obj

{-# INLINE isNotBoundIn #-}
isNotBoundIn :: (HasBound b) => Name -> b -> Bool
isNotBoundIn name obj = name `notElem` boundIn obj

{-# INLINE appearsFreeIn #-}
appearsFreeIn :: (HasFree a t) => Name -> a -> Bool
appearsFreeIn name = appearsIn name . freeIn

{-# INLINE appearsIn #-}
appearsIn :: Name -> Set (Label t) -> Bool
appearsIn name set = name `elem` Set.map labelName set
