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
--) where

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

-- TODO
type W a = [(Name, Dictionary (IndexedPattern a), Maybe (IndexedPattern a))]

newtype RecordPatternStack a p = RecordPatternStack {recordPatternStack :: RWS Name (W a) Int p}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Name
    , MonadWriter (W a)
    , MonadState Int
    , MonadRWS Name (W a) Int
    )

compileRecordPatterns :: forall a k t. (Monoid a, Show a, Data a, Data t, Data k) => Module a k t -> RecordPatternStack a (Module a k t)
compileRecordPatterns = transformBiM (expandRecordPatterns @a @(Expression a (Type TypeIndex Kind)))

class RecordPattern a p where
  expandRecordPatterns :: p -> RecordPatternStack a p

{-# INLINE runExpandRecordPatterns #-}
runExpandRecordPatterns :: RecordPatternStack a p -> Name -> Int -> (p, Int)
runExpandRecordPatterns a n s = (fst_, snd_)
 where
  (fst_, snd_, _) = runRWS (recordPatternStack a) n s

instance (RecordPattern a p) => RecordPattern a [p] where
  expandRecordPatterns = traverse expandRecordPatterns

instance (RecordPattern a p) => RecordPattern a (List1 p) where
  expandRecordPatterns = traverse expandRecordPatterns

instance (RecordPattern a p) => RecordPattern a (Map k p) where
  expandRecordPatterns = traverse expandRecordPatterns

instance (RecordPattern a p) => RecordPattern a (Maybe p) where
  expandRecordPatterns = traverse expandRecordPatterns

instance (Data a, Monoid a, Show a) => RecordPattern a (Expression a IndexedType) where
  expandRecordPatterns =
    \case
      EMatch a t e cs ->
        EMatch a t e <$> expandRecordPatterns cs
      EFold a t es cs e ->
        EFold a t es <$> expandRecordPatterns cs <*> pure e
      e ->
        pure e

instance (Data a, Monoid a, Show a) => RecordPattern a (Guard Expression a IndexedType) where
  expandRecordPatterns =
    \case
      CGuard e ->
        CGuard <$> expandRecordPatterns e

instance (Data a, Monoid a, Show a) => RecordPattern a (Clause a IndexedType) where
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

instance (Monoid a, Show a) => RecordPattern a (Pattern a IndexedType) where
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

--

type RecordInfo a = (Name, Dictionary (IndexedPattern a), Maybe (IndexedPattern a))

newtype BorkStack a p = BorkStack {borkStack :: RWS Name [RecordInfo a] Int p}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Name
    , MonadWriter (W a)
    , MonadState Int
    , MonadRWS Name [RecordInfo a] Int
    )

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
                  ( CPlain
                      mempty
                      []
                      ( EMatch
                          mempty
                          t1
                          (EVariable mempty ll1)
                          ( EClause
                              mempty
                              ( PConstructor
                                  mempty
                                  (Label (TIntrinsic (IRecord (TRow row))) "$Record")
                                  [PVariable 
                                    mempty 
                                    (Label (TRow row) var)
                                  ]
                              )
                              (CPlain mempty [] expr :| [])
                              :| []
                          )
                      )
                      :| []
                  )
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
