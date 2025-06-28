{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Pattern.RecordDesugar where

import Control.Monad.RWS
import Control.Monad.State (MonadState)
import Control.Monad.Writer
import Data.Data (Data)
import Data.Foldable (foldrM)
import Data.Generics.Uniplate.Data (transformBiM)
import Lang.Common.List1 (List1, NonEmpty (..))
import Lang.Common.Supply (suppliedName)
import Lang.Label (Label (..))
import Lang.Utils (Dictionary, Map, Name, traverseM)
import Noll.Language
import Noll.Language.HasType (HasType (..))
import Noll.Language.Type.Row (RowData (..))
import Noll.Module (Module (..))

import qualified Data.Map.Strict as Map

compileRecordPatterns ::
  forall m a k t.
  (Monoid a, Show a, Data a, Data k, Data t, MonadState Int m, MonadWriter [(Name, Dictionary (TypedPattern a), Maybe (TypedPattern a))] m, MonadReader Name m) =>
  Module a k t ->
  m (Module a k t)
compileRecordPatterns = transformBiM (expandRecordPatterns :: Expression a (Type TypeIndex Kind) -> m (Expression a (Type TypeIndex Kind)))

compileRecordPatterns2 ::
  forall m a.
  (Monoid a, Show a, Data a, MonadState Int m, MonadWriter [(Name, Dictionary (TypedPattern a), Maybe (TypedPattern a))] m, MonadReader Name m) =>
  Expression a (Type TypeIndex Kind) ->
  m (Expression a (Type TypeIndex Kind))
compileRecordPatterns2 = transformBiM (expandRecordPatterns :: Expression a (Type TypeIndex Kind) -> m (Expression a (Type TypeIndex Kind)))

-- expandExpression :: (Show a, Data a, Monoid a, MonadState Int m, MonadWriter [(Name, Dictionary (TypedPattern a), Maybe (TypedPattern a))] m, MonadReader Name m) => Expression a (Type TypeIndex Kind) -> m (Expression a (Type TypeIndex Kind))
-- expandExpression =
--  \case
--      EMatch a t e cs ->
--        EMatch a t e <$> expandRecordPatterns cs
--      EFold a t es cs e ->
--        EFold a t es <$> expandRecordPatterns cs <*> pure e
--      e ->
--        pure e

type TypedPattern a = Pattern a (Type TypeIndex Kind)

class RecordPattern a p where
  expandRecordPatterns :: (MonadState Int m, MonadWriter [(Name, Dictionary (TypedPattern a), Maybe (TypedPattern a))] m, MonadReader Name m) => p -> m p

runExpandRecordPatterns :: RWS Name w Int a -> Name -> Int -> (a, w)
runExpandRecordPatterns = evalRWS

instance (RecordPattern a p) => RecordPattern a [p] where
  expandRecordPatterns = traverse expandRecordPatterns

instance (RecordPattern a p) => RecordPattern a (List1 p) where
  expandRecordPatterns = traverse expandRecordPatterns

instance (RecordPattern a p) => RecordPattern a (Map k p) where
  expandRecordPatterns = traverse expandRecordPatterns

instance (RecordPattern a p) => RecordPattern a (Maybe p) where
  expandRecordPatterns = traverse expandRecordPatterns

instance (Data a, Monoid a, Show a) => RecordPattern a (Expression a (Type TypeIndex Kind)) where
  expandRecordPatterns =
    \case
      EMatch a t e cs ->
        EMatch a t e <$> expandRecordPatterns cs
      EFold a t es cs e ->
        EFold a t es <$> expandRecordPatterns cs <*> pure e
      e ->
        pure e

instance (Data a, Monoid a, Show a) => RecordPattern a (Guard Expression a (Type TypeIndex Kind)) where
  expandRecordPatterns =
    \case
      CGuard e ->
        CGuard <$> expandRecordPatterns e

instance (Data a, Monoid a, Show a) => RecordPattern a (Clause a (Type TypeIndex Kind)) where
  expandRecordPatterns =
    \case
      EClause a p cs -> do
        (q, ys) <- runWriterT (expandRecordPatterns p)
        ds <- forM cs $
          \case
            CPlain a gs e -> do
              hs <- expandRecordPatterns gs
              f <- foldrM zork e ys
              pure (CPlain a hs f)
            CLambda{} ->
              error "Not implemented"
        pure (EClause a q ds)

zork ::
  (Data a, Monoid a, MonadState Int m, MonadReader Name m) =>
  (Name, Dictionary (TypedPattern a), Maybe (TypedPattern a)) ->
  Expression a (Type TypeIndex Kind) ->
  m (Expression a (Type TypeIndex Kind))
zork (name, d, mp) e = do
  names <- replicateM (length zz - 1) suppliedName
  (_, _, aa) <- foldrM tork ("_", RNil, e) (zip zz (name : names))
  pure aa
 where
  zz = Map.toList d

tork ::
  (Data a, Monad m, Monoid a) =>
  ((Name, TypedPattern a), Name) ->
  (Name, Row TypeIndex Kind (Type TypeIndex Kind), Expression a (Type TypeIndex Kind)) ->
  m (Name, Row TypeIndex Kind (Type TypeIndex Kind), Expression a (Type TypeIndex Kind))
tork ((name, p), rrr) (x, tttr, e) = do
  pure
    ( rrr
    , RExtend name q tttr
    , EFocus
        name
        (Label q (rrr <> ".field." <> name))
        (Label (TIntrinsic (IRecord (TRow tttr))) (rrr <> ".tail"))
        (EVariable mempty (Label (TRow (RExtend name q tttr)) rrr))
        ( EMatch
            mempty
            t
            (EVariable mempty (Label q (rrr <> ".field." <> name)))
            ( EClause
                mempty
                p
                ( CPlain
                    mempty
                    []
                    ( EMatch
                        mempty
                        t
                        (EVariable mempty (Label (TIntrinsic (IRecord (TRow tttr))) (rrr <> ".tail")))
                        ( EClause
                            mempty
                            (PConstructor mempty (Label (TIntrinsic (IRecord (TRow tttr))) "$Record") [PVariable mempty (Label (TRow tttr) x)])
                            (CPlain mempty [] e :| [])
                            :| []
                        )
                    )
                    :| []
                )
                :| []
            )
        )
    )
 where
  t = typeOf e :: Type TypeIndex Kind
  q = typeOf p :: Type TypeIndex Kind

instance (Monoid a, Show a) => RecordPattern a (Pattern a (Type TypeIndex Kind)) where
  expandRecordPatterns =
    \case
      PAnnotation a t p ->
        PAnnotation a t <$> expandRecordPatterns p
      PConstructor a ll ps ->
        PConstructor a ll <$> traverse expandRecordPatterns ps
      PRecord _ t@(TIntrinsic (IRecord r)) d p -> do
        name <- suppliedName
        tell [(name, d, p)]
        pure (PConstructor mempty (Label t "$Record") [PVariable mempty (Label r name)])
      PListCons a t p1 p2 ->
        PListCons a t <$> expandRecordPatterns p1 <*> expandRecordPatterns p2
      PListLiteral a t ps ->
        PListLiteral a t <$> traverse expandRecordPatterns ps
      p ->
        pure p
