{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Unification (
  Unifier (..),
  Unifiable (unify),
  UnificationError (..),
  unifyAll,
  runUnifier,
) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.State (MonadState, StateT, evalStateT)
import Data.List.NonEmpty (NonEmpty, (<|))
import Data.Set (member)
import Debug.Trace
import Noll.Common.Supply (supplied)
import Noll.Language (
  IndexedType,
  Intrinsic (..),
  Kind (..),
  Row (..),
  Type (..),
  TypeIndex (..),
  extractField,
  kindOf,
  typeIdsIn,
  updateTail,
 )
import Noll.TypeSystem.Substitution (
  Substitutable (..),
  Substitution (..),
  mapsTo,
 )
import Noll.Utils (foldrM)

import qualified Data.List.NonEmpty as NonEmpty

newtype Unifier a = Unifier {unifierStack :: StateT Int (Either UnificationError) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState Int
    , MonadError UnificationError
    )

{-# INLINE runUnifier #-}
runUnifier :: Int -> Unifier a -> Either UnificationError a
runUnifier n u = evalStateT (unifierStack u) n

data UnificationError
  = CannotUnify
  | InfiniteType
  | CannotUnifyKinds
  deriving (Show, Eq, Ord, Read)

class Unifiable u where
  unify :: (MonadState Int m, MonadError UnificationError m) => u -> u -> m Substitution

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
  unify RNil RNil =
    pure mempty
  unify (RVariable v) row2 =
    bindType v (TRow row2)
  unify row1 (RVariable v) =
    bindType v (TRow row1)
  unify row1@(RExtend name _ _) row2@(RExtend _ _ q1) =
    case extractField name row1 of
      Just (t1, r1) ->
        case extractField name row2 of
          Just (t2, r2) -> do
            sub1 <- unify r1 r2
            sub2 <- unify (apply sub1 t1) (apply sub1 t2)
            pure (sub2 <> sub1)
          Nothing -> do
            r2 <- supplied (RVariable . TypeIndex KRow)
            sub1 <- unify q1 (RExtend name t1 r2)
            sub2 <- unify (apply sub1 r1) (apply sub1 (updateTail r2 row2))
            pure (sub2 <> sub1)
      Nothing ->
        error "Implementation error"
  unify _ _ =
    throwError CannotUnify

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
bindType (TypeIndex k index) =
  \case
    TVariable (TypeIndex k2 index2)
      | index == index2 ->
          if k /= k2
            then throwError CannotUnifyKinds
            else pure mempty
    t
      | index `member` typeIdsIn t ->
          throwError InfiniteType
      | k /= kindOf t ->
          throwError CannotUnifyKinds
      | otherwise ->
          pure (index `mapsTo` t)

unifyAll :: (Show u, MonadState Int m, MonadError UnificationError m, Unifiable u) => [u] -> m Substitution
unifyAll [] = pure mempty
unifyAll (t : ts) = do
  sub1 <- foldrM go mempty ts
  sub2 <- unifyAll ts
  pure (sub2 <> sub1)
 where
  go t1 sub1 = do
    sub2 <- unify t t1
    pure (sub2 <> sub1)
