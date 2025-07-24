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
import Data.Tuple.Extra (thd3)
import Lang.Common.List1 (List1, NonEmpty (..))
import Lang.Common.Supply (suppliedName)
import Lang.Label (Label (..))
import Lang.Utils (Dictionary, Map, Name)
import Noll.Language
import Noll.Language.Module (Module (..))

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
      EClause a pat clauses -> do
        (pat', fields) <- listen (expandRecordPatterns pat)
        clauses' <- forM clauses $ \case
          CPlain a1 guards expr -> do
            guards' <- expandRecordPatterns guards
            expr' <- foldrM (insertMatch a1) expr fields
            pure (CPlain a1 guards' expr')
          CLambda{} ->
            error "Not implemented"
        pure (EClause a pat' clauses')
   where
    insertMatch a (name, dict, _) expr = do
      let fields = Map.toList dict
      names <- replicateM (length fields - 1) suppliedName
      thd3 <$> foldrM (go a) ("_", RNil, expr) (zip fields (name : names))

    go _ ((fieldName, pat), prefix) (var, row, expr) = do
      let t1 = typeOf expr
          t2 = typeOf pat
          ll1 = Label t2 (prefix <> ".field." <> fieldName)
          ll2 = Label (TIntrinsic (IRecord (TRow row))) (prefix <> ".tail")
          var1 = EVariable mempty (Label (TRow (RExtend fieldName t2 row)) prefix)
          var2 = EVariable mempty ll2

          innerClause =
            EClause
              mempty
              ( PConstructor
                  mempty
                  (Label (TIntrinsic (IRecord (TRow row))) "$Record")
                  [PVariable mempty (Label (TRow row) var)]
              )
              (CPlain mempty [] expr :| [])

          matchTail =
            EMatch mempty t1 var2 (innerClause :| [])

          matchField =
            EMatch
              mempty
              t1
              (EVariable mempty ll1)
              (EClause mempty pat (CPlain mempty [] matchTail :| []) :| [])

          focusExpr =
            EFocus fieldName ll1 ll2 var1 matchField

      pure (prefix, RExtend fieldName t2 row, focusExpr)

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
