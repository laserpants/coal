{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.TypeUnification (TypeUnifiable (..), unifyAll) where

import Control.Monad.Except
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (member)
import Noll.Language (
  Intrinsic (..),
  Kind (..),
  KindIndex (..),
  OpaqueRow,
  OpaqueType,
  Row,
  Type (..),
  TypeIndex (..),
  typeIdsIn,
 )
import Noll.TypeSystem.TypeSubstitution (
  TypeSubstitutable (..),
  TypeSubstitution (..),
  mapsToType,
 )
import Noll.TypeSystem.Unification.Error (UnificationError (..))
import qualified Noll.TypeSystem.Unification.Error as Error
import Noll.Utils (foldrM)

class TypeUnifiable u where
  unify :: (MonadError UnificationError m) => u -> u -> m TypeSubstitution

instance (TypeSubstitutable u, TypeUnifiable u) => TypeUnifiable [u] where
  unify [] [] =
    pure mempty
  unify (u1 : us1) (u2 : us2) = do
    sub1 <- unify u1 u2
    sub2 <- unify (apply sub1 us1) (apply sub1 us2)
    pure (sub2 <> sub1)
  unify _ _ =
    error "Implementation error"

instance (TypeSubstitutable u, TypeUnifiable u) => TypeUnifiable (NonEmpty u) where
  unify u1 u2 = unify (NonEmpty.toList u1) (NonEmpty.toList u2)

instance TypeUnifiable OpaqueRow where
  unify =
    error "TODO"

instance TypeUnifiable OpaqueType where
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
    unify (t1 : NonEmpty.toList ts1) (t2 : NonEmpty.toList ts2)
  unify (TConstructor _ c1) (TConstructor _ c2)
    | c1 == c2 =
        pure mempty
  unify (TRow r1) (TRow r2) =
    unify r1 r2
  unify (TIntrinsic t1) (TIntrinsic t2) =
    unify t1 t2
  unify _ _ =
    throwError Error.CannotUnify

instance TypeUnifiable (Intrinsic OpaqueType) where
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
    throwError Error.CannotUnify

bindType :: (MonadError UnificationError m) => TypeIndex () -> OpaqueType -> m TypeSubstitution
bindType (TypeIndex _ index) =
  \case
    TVariable (TypeIndex _ index2)
      | index == index2 ->
          pure mempty
    t
      | index `member` typeIdsIn t ->
          throwError Error.InfiniteType
      | otherwise ->
          pure (index `mapsToType` t)

unifyAll :: (MonadError UnificationError m, TypeUnifiable u) => [u] -> m TypeSubstitution
unifyAll [] = pure mempty
unifyAll (t : ts) = do
  sub1 <- foldrM go mempty ts
  sub2 <- unifyAll ts
  pure (sub2 <> sub1)
 where
  go t1 sub1 = do
    sub2 <- unify t t1
    pure (sub2 <> sub1)
