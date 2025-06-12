{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.SystemF.Unification (
  Unifier (..),
  Unifiable (unify, match),
  UnificationError (..),
  unifyAll,
  runUnifier,
  evalUnifier,
) where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.State (MonadState, State, runState)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty, (<|))
import Data.Set (member)
import Lang.Common.Supply (supplied)
import Lang.Utils (foldrM, (<$$>))
import Noll.Language
import Noll.SystemF.Substitution (Substitutable (..), Substitution (..), mapsTo, merge)

import qualified Data.List.NonEmpty as NonEmpty

newtype Unifier a = Unifier {unifierStack :: ExceptT UnificationError (State Int) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState Int
    , MonadError UnificationError
    )

{-# INLINE runUnifier #-}
runUnifier :: Int -> Unifier a -> (Either UnificationError a, Int)
runUnifier n u = runState (runExceptT (unifierStack u)) n

{-# INLINE evalUnifier #-}
evalUnifier :: Int -> Unifier a -> Either UnificationError a
evalUnifier = fst <$$> runUnifier

data UnificationError
  = ECannotUnify
  | ECannotMatch
  | EInfiniteType
  | EKindMismatch
  deriving (Show, Eq, Ord, Read)

class Unifiable u where
  unify, match :: u -> u -> Unifier Substitution

instance (Substitutable u, Data u, Unifiable u) => Unifiable [u] where
  unify [] [] =
    pure mempty
  unify (t1 : ts1) (t2 : ts2) = do
    sub1 <- unify t1 t2
    sub2 <- unify (apply sub1 ts1) (apply sub1 ts2)
    pure (sub2 <> sub1)
  unify _ _ =
    error "Implementation error"

  match [] [] =
    pure mempty
  match (t1 : ts1) (t2 : ts2) = do
    sub1 <- match t1 t2
    sub2 <- match ts1 ts2
    maybe (throwError ECannotMatch) pure (merge sub1 sub2)
  match _ _ =
    error "Implementation error"

instance (Substitutable u, Unifiable u, Data u) => Unifiable (NonEmpty u) where
  unify t1 t2 = unify (NonEmpty.toList t1) (NonEmpty.toList t2)
  match t1 t2 = match (NonEmpty.toList t1) (NonEmpty.toList t2)

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
    throwError ECannotUnify

  match (IList t1) (IList t2) =
    match t1 t2
  match (IOption t1) (IOption t2) =
    match t1 t2
  match (IRecord t1) (IRecord t2) =
    match t1 t2
  match (IResult t1) (IResult t2) =
    match t1 t2
  match (ITuple ts1) (ITuple ts2) =
    match ts1 ts2
  match t1 t2
    | t1 == t2 =
        pure mempty
  match _ _ =
    throwError ECannotMatch

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
    throwError ECannotUnify

  match RNil RNil =
    pure mempty
  match (RVariable (TypeIndex _ t)) row2 =
    pure (t `mapsTo` TRow row2)
  match row1@(RExtend name _ _) row2@(RExtend _ _ q1) =
    case extractField name row1 of
      Just (t1, r1) ->
        case extractField name row2 of
          Just (t2, r2) -> do
            sub1 <- match r1 r2
            sub2 <- match t1 t2
            maybe (throwError ECannotMatch) pure (merge sub1 sub2)
          Nothing -> do
            error "Not implemented"
      -- r2 <- freshRow
      -- sub1 <- match q1 (RExtend name t1 r2)
      -- sub2 <- match r1 (updateRowTail r2 row2)
      -- maybe (throwError ECannotMatch) pure (merge sub1 sub2)
      Nothing ->
        error "Implementation error"
  match _ _ =
    throwError ECannotMatch

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
    throwError ECannotUnify

  match (TAlias _ _ t1) t2 =
    match t1 t2
  match t1 (TAlias _ _ t2) =
    match t1 t2
  match (TVariable (TypeIndex _ t)) t2 =
    pure (t `mapsTo` t2)
  match (TArrow t1 u1) (TArrow t2 u2) =
    match [t1, u1] [t2, u2]
  match (TApplication _ t1 ts1) (TApplication _ t2 ts2) = do
    match (t1 <| ts1) (t2 <| ts2)
  match (TRow r1) (TRow r2) =
    match r1 r2
  match (TConstructor _ c1) (TConstructor _ c2)
    | c1 == c2 =
        pure mempty
  match (TIntrinsic t1) (TIntrinsic t2) =
    match t1 t2
  match _ _ =
    throwError ECannotMatch

bindType :: TypeIndex Kind -> IndexedType -> Unifier Substitution
bindType (TypeIndex k1 ix1) =
  \case
    TVariable (TypeIndex k2 ix2)
      | ix1 == ix2 ->
          if k1 /= k2
            then throwError EKindMismatch
            else pure mempty
    t
      | ix1 `member` typeIdsIn t ->
          throwError EInfiniteType
      | k1 /= kindOf t ->
          throwError EKindMismatch
      | otherwise ->
          pure (ix1 `mapsTo` t)

unifyAll :: (Unifiable u) => [u] -> Unifier Substitution
unifyAll [] = pure mempty
unifyAll (t : ts) = do
  sub1 <- foldrM go mempty ts
  sub2 <- unifyAll ts
  pure (sub2 <> sub1)
 where
  go t1 sub1 = do
    sub2 <- unify t t1
    pure (sub2 <> sub1)
