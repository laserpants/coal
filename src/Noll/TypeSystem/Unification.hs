{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Unification (
  Unifiable (..),
  UnificationError (..),
  unifyAll,
) where

import Control.Monad.Except (MonadError, throwError)
import Data.List.NonEmpty (NonEmpty, (<|))
import Data.Set (member)
import Noll.Language (
  IndexedType,
  Intrinsic (..),
  Kind (..),
  Row (..),
  Type (..),
  TypeIndex (..),
  typeIdsIn,
 )
import Noll.TypeSystem.Substitution (
  Substitutable (..),
  Substitution (..),
  mapsTo,
 )
import Noll.Utils (foldrM)

import qualified Data.List.NonEmpty as NonEmpty

data UnificationError
  = CannotUnify
  | InfiniteType
  | CannotUnifyKinds
  deriving (Show, Eq, Ord, Read)

class Unifiable u where
  unify :: (MonadError UnificationError m) => u -> u -> m Substitution

instance (Substitutable u, Unifiable u) => Unifiable [u] where
  unify [] [] =
    pure mempty
  unify (u1 : us1) (u2 : us2) = do
    sub1 <- unify u1 u2
    sub2 <- unify (apply sub1 us1) (apply sub1 us2)
    pure (sub2 <> sub1)
  unify _ _ =
    error "Implementation error"

instance (Substitutable u, Unifiable u) => Unifiable (NonEmpty u) where
  unify u1 u2 = unify (NonEmpty.toList u1) (NonEmpty.toList u2)

instance Unifiable (Intrinsic IndexedType) where
  unify (IList t1) (IList t2) =
    unify t1 t2
  unify (IOption t1) (IOption t2) =
    unify t1 t2
  unify (IRecord t1) (IRecord t2) =
    unify t1 t2
  unify (IResult t1) (IResult t2) =
    unify t1 t2
  unify (ITuple ts1) (ITuple ts2) =
    unify ts1 ts2
  unify t1 t2
    | t1 == t2 =
        pure mempty
  unify _ _ =
    throwError CannotUnify

instance Unifiable (Row TypeIndex Kind IndexedType) where
  unify =
    error "TODO"

instance Unifiable IndexedType where
  unify (TAlias _ _ t1) t2 =
    unify t1 t2
  unify t1 (TAlias _ _ t2) =
    unify t1 t2
  unify (TVariable t) t2 =
    bindType t t2
  unify t1 (TVariable t) =
    bindType t t1
  unify (TArrow t1 u1) (TArrow t2 u2) =
    unify [t1, u1] [t2, u2]
  unify (TApplication _ t1 ts1) (TApplication _ t2 ts2) =
    unify (t1 <| ts1) (t2 <| ts2)
  unify (TConstructor _ c1) (TConstructor _ c2)
    | c1 == c2 =
        pure mempty
  unify (TRow r1) (TRow r2) =
    unify r1 r2
  unify (TIntrinsic t1) (TIntrinsic t2) =
    unify t1 t2
  unify _ _ =
    throwError CannotUnify

bindType :: (MonadError UnificationError m) => TypeIndex Kind -> IndexedType -> m Substitution
bindType (TypeIndex _ index) =
  \case
    TVariable (TypeIndex _ index2)
      | index == index2 ->
          pure mempty
    t
      | index `member` typeIdsIn t ->
          throwError InfiniteType
      | otherwise ->
          pure (index `mapsTo` t)

unifyAll :: (MonadError UnificationError m, Unifiable u) => [u] -> m Substitution
unifyAll [] = pure mempty
unifyAll (t : ts) = do
  sub1 <- foldrM go mempty ts
  sub2 <- unifyAll ts
  pure (sub2 <> sub1)
 where
  go t1 sub1 = do
    sub2 <- unify t t1
    pure (sub2 <> sub1)
