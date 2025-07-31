{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Coal.Compiler.Transform.Pattern.RecordDesugar where -- (
--  RecordPattern (..),
--  RecordPatternStack (..),
--  IndexedPattern,
--  compileRecordPatterns,
--  runExpandRecordPatterns,
-- ) where

import Coal.Common.Label (Label (..))
import Coal.Common.List1 (List1, NonEmpty (..))
import Coal.Common.Supply (suppliedName)
import Coal.Language
import Coal.Language.Module (Module (..))
import Control.Monad.RWS
import Data.Data (Data)
import Data.Foldable (foldrM)
import Data.Generics.Uniplate.Data (transformBiM)
import Data.Tuple.Extra (thd3)
import Extra (Dictionary, Map, Name)

import qualified Data.Map.Strict as Map

type IndexedPattern a = Pattern a IndexedType

borkRecordPatterns :: (Data a, Monoid a) => Module a Kind IndexedType -> BorkStack a (Module a Kind IndexedType)
borkRecordPatterns = transformBiM borkRecordsExpression

type RecordInfo a = (Name, Dictionary (IndexedPattern a), Maybe (IndexedPattern a))

newtype BorkStack a p = BorkStack {borkStack :: RWS Name [RecordInfo a] Int p}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Name
    , MonadWriter [RecordInfo a]
    , MonadState Int
    , MonadRWS Name [RecordInfo a] Int
    )

evalBorkStack :: BorkStack a p -> Name -> Int -> (p, Int)
evalBorkStack a n s = (p, m) where (p, m, _) = runRWS (borkStack a) n s

runBorkStack :: BorkStack a p -> Name -> Int -> (p, Int, [RecordInfo a])
runBorkStack a = runRWS (borkStack a)

borkRecordsExpression :: (Data a, Monoid a) => Expression a IndexedType -> BorkStack a (Expression a IndexedType)
borkRecordsExpression =
  \case
    EMatch a t e cs ->
      EMatch a t e <$> traverse borkRecordsClause cs
    EFold a t es cs e ->
      EFold a t es cs <$> traverse borkRecordsExpression e
    EUnfold a t ll n ps d me ->
      EUnfold a t ll n ps d <$> traverse borkRecordsExpression me
    e ->
      pure e

borkRecordsGuard :: (Data a, Monoid a) => Guard Expression a IndexedType -> BorkStack a (Guard Expression a IndexedType)
borkRecordsGuard =
  \case
    CGuard e ->
      CGuard <$> borkRecordsExpression e

borkRecordsClause :: (Data a, Monoid a) => Clause a IndexedType -> BorkStack a (Clause a IndexedType)
borkRecordsClause =
  \case
    EClause a p cs -> do
      (q, fs) <- listen (borkRecordsPattern p)
      ds <- forM cs $
        \case
          CPlain a1 gs e -> do
            hs <- traverse borkRecordsGuard gs
            e1 <- foldrM borkbork e fs
            pure (CPlain a1 hs e1)
          CLambda{} ->
            error "Not implemented"
      pure (EClause a q ds)

borkbork :: (Data a, Monoid a) => RecordInfo a -> Expression a IndexedType -> BorkStack a (Expression a IndexedType)
borkbork (name, dict, _) expr = do
  names <- replicateM (length fields - 1) suppliedName
  (_, _, xx) <- foldrM go ("_", RNil, expr) (zip fields (name : names))
  pure xx
 where
  fields = Map.toList dict

go :: (Data a, Monoid a) => ((Name, IndexedPattern a), Name) -> (Name, Row TypeIndex Kind IndexedType, Expression a IndexedType) -> BorkStack a (Name, Row TypeIndex Kind IndexedType, Expression a IndexedType)
go ((fname, p), prefix) (var, row, expr) = do
  let t1 = typeOf expr
      t2 = typeOf p
      ll1 = Label t2 (prefix <> ".field." <> fname)
      ll2 = Label (TIntrinsic (IRecord (TRow row))) (prefix <> ".tail")
      var1 = EVariable mempty (Label (TRow (RExtend fname t2 row)) prefix)
      var2 = EVariable mempty ll2

  e2 <-
    borkRecordsExpression
      ( EMatch
          mempty
          t1
          (EVariable mempty ll1)
          ( EClause
              mempty
              p
              (CPlain mempty [] expr :| [])
              :| []
          )
      )

  let focusExpr =
        EFocus
          fname
          ll1
          ll2
          var1
          ( EMatch
              mempty
              t1
              var2
              ( EClause
                  mempty
                  ( PConstructor
                      mempty
                      (Label (TIntrinsic (IRecord (TRow row))) "$Record")
                      [PVariable mempty (Label (TRow row) var)]
                  )
                  (CPlain mempty [] e2 :| [])
                  :| []
              )
          )

  pure (prefix, RExtend fname t2 row, focusExpr)

borkRecordsPattern :: (Data a, Monoid a) => Pattern a IndexedType -> BorkStack a (Pattern a IndexedType)
borkRecordsPattern =
  \case
    PAnnotation a t p ->
      PAnnotation a t <$> borkRecordsPattern p
    PConstructor a ll ps ->
      PConstructor a ll <$> traverse borkRecordsPattern ps
    PRecord _ t@(TIntrinsic (IRecord r)) d p -> do
      name <- suppliedName
      tell [(name, d, p)]
      pure (PConstructor mempty (Label t "$Record") [PVariable mempty (Label r name)])
    PListCons a t p1 p2 ->
      PListCons a t <$> borkRecordsPattern p1 <*> borkRecordsPattern p2
    PListLiteral a t ps ->
      PListLiteral a t <$> traverse borkRecordsPattern ps
    p ->
      pure p
