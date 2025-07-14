{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Noll.Compiler.Transform.Pattern.RecordDesugar (
  RecordPattern (..),
  TypedPattern,
  compileRecordPatterns,
  runExpandRecordPatterns,
) where

import Control.Monad.RWS
import Data.Data (Data)
import Data.Foldable (foldrM)
import Data.Generics.Uniplate.Data (transformBiM)
import Lang.Common.List1 (List1, NonEmpty (..))
import Lang.Common.Supply (suppliedName)
import Lang.Label (Label (..))
import Lang.Utils (Dictionary, Map, Name)
import Noll.Language
import Noll.Module (Module (..))

import qualified Data.Map.Strict as Map

type TypedPattern a = Pattern a (Type TypeIndex Kind)

compileRecordPatterns ::
  forall a k t.
  (Monoid a, Show a, Data a, Data t, Data k) =>
  Module a k t ->
  RWS Name [(Name, Dictionary (TypedPattern a), Maybe (TypedPattern a))] Int (Module a k t)
compileRecordPatterns = transformBiM (expandRecordPatterns @a @(Expression a (Type TypeIndex Kind)))

class RecordPattern a p where
  expandRecordPatterns :: p -> RWS Name [(Name, Dictionary (TypedPattern a), Maybe (TypedPattern a))] Int p

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
        (q, ys) <- listen (expandRecordPatterns p)
        ds <- forM cs $
          \case
            CPlain a1 gs e -> do
              hs <- expandRecordPatterns gs
              f <- foldrM go e ys
              pure (CPlain a1 hs f)
            CLambda{} ->
              error "Not implemented"
        pure (EClause a q ds)
   where
    go (name, d, _) e = do
      let fields = Map.toList d
      names <- replicateM (length fields - 1) suppliedName
      (_, _, e1) <- foldrM go1 ("_", RNil, e) (zip fields (name : names))
      pure e1
    go1 ((name, p), pfix) (var, row, e) = do
      let t = typeOf e
          q = typeOf p
          ll = Label q (pfix <> ".field." <> name)
      pure
        ( pfix
        , RExtend name q row
        , EFocus
            name
            ll
            (Label (TIntrinsic (IRecord (TRow row))) (pfix <> ".tail"))
            (EVariable mempty (Label (TRow (RExtend name q row)) pfix))
            ( EMatch
                mempty
                t
                (EVariable mempty ll)
                ( EClause
                    mempty
                    p
                    ( CPlain
                        mempty
                        []
                        ( EMatch
                            mempty
                            t
                            (EVariable mempty (Label (TIntrinsic (IRecord (TRow row))) (pfix <> ".tail")))
                            ( EClause
                                mempty
                                (PConstructor mempty (Label (TIntrinsic (IRecord (TRow row))) "$Record") [PVariable mempty (Label (TRow row) var)])
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
