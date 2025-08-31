{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

-- TODO
module Coal.Compiler.Transform.Pattern.RecordDesugar (
  RecordDesugarStack (..),
  RecordDesugarable (..),
  compileRecordPatterns,
  evalRecordDesugarStack,
  runRecordDesugarStack,
) where

import Coal.Common.Label (Label (..))
import Coal.Common.Supply (suppliedName)
import Coal.Language
import Coal.Language.Module (Module (..))
import Control.Monad.RWS
import Data.Data (Data)
import Data.Foldable (foldrM)
import Data.Generics.Uniplate.Data (transformBiM)
import Data.List.NonEmpty (NonEmpty (..))
import Extra (Dictionary, Name)

import qualified Data.Map.Strict as Map

type IndexedPattern a = Pattern a IndexedType

compileRecordPatterns :: forall a. (Data a, Monoid a) => Module a Kind IndexedType -> RecordDesugarStack a (Module a Kind IndexedType)
compileRecordPatterns = transformBiM (desugarRecordPatterns @a @(Expression a (Type TypeIndex Kind)))

type RecordInfo a = (Name, Dictionary (IndexedPattern a), Maybe (IndexedPattern a))

newtype RecordDesugarStack a p = RecordDesugarStack {desugarRecordPatternsStack :: RWS Name [RecordInfo a] Int p}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Name
    , MonadWriter [RecordInfo a]
    , MonadState Int
    , MonadRWS Name [RecordInfo a] Int
    )

{-# INLINE evalRecordDesugarStack #-}
evalRecordDesugarStack :: RecordDesugarStack a p -> Name -> Int -> (p, Int)
evalRecordDesugarStack a n s = (p, m) where (p, m, _) = runRWS (desugarRecordPatternsStack a) n s

{-# INLINE runRecordDesugarStack #-}
runRecordDesugarStack :: RecordDesugarStack a p -> Name -> Int -> (p, Int, [RecordInfo a])
runRecordDesugarStack a = runRWS (desugarRecordPatternsStack a)

class RecordDesugarable a p where
  desugarRecordPatterns :: p -> RecordDesugarStack a p

instance (RecordDesugarable a p) => RecordDesugarable a (Maybe p) where
  desugarRecordPatterns = traverse desugarRecordPatterns

instance (RecordDesugarable a p) => RecordDesugarable a [p] where
  desugarRecordPatterns = traverse desugarRecordPatterns

instance (RecordDesugarable a p) => RecordDesugarable a (NonEmpty p) where
  desugarRecordPatterns = traverse desugarRecordPatterns

instance (Data a, Monoid a) => RecordDesugarable a (Expression a IndexedType) where
  desugarRecordPatterns =
    \case
      EMatch a t e cs ->
        EMatch a t e <$> desugarRecordPatterns cs
      EFold a t es cs e ->
        EFold a t es cs <$> desugarRecordPatterns e
      EUnfold a t n ps d me ->
        EUnfold a t n ps d <$> desugarRecordPatterns me
      e ->
        pure e

instance (Data a, Monoid a) => RecordDesugarable a (Guard Expression a IndexedType) where
  desugarRecordPatterns =
    \case
      CGuard e ->
        CGuard <$> desugarRecordPatterns e

instance (Data a, Monoid a) => RecordDesugarable a (Clause a IndexedType) where
  desugarRecordPatterns =
    \case
      EClause a p cs -> do
        (q, fs) <- listen (desugarRecordPatterns p)
        ds <- forM cs $
          \case
            CPlain a1 gs e -> do
              hs <- desugarRecordPatterns gs
              e1 <- foldrM desugar e fs
              pure (CPlain a1 hs e1)
            CLambda{} ->
              error "Not implemented"
        pure (EClause a q ds)

instance (Data a, Monoid a) => RecordDesugarable a (Pattern a IndexedType) where
  desugarRecordPatterns =
    \case
      PAnnotation a t p ->
        PAnnotation a t <$> desugarRecordPatterns p
      PConstructor a ll ps ->
        PConstructor a ll <$> desugarRecordPatterns ps
      PRecord _ t@(TIntrinsic (IRecord r)) d p -> do
        name <- suppliedName
        tell [(name, d, p)]
        pure (PConstructor mempty (Label t "$Record") [PVariable mempty (Label r name)])
      PListCons a t p1 p2 ->
        PListCons a t <$> desugarRecordPatterns p1 <*> desugarRecordPatterns p2
      PListLiteral a t ps ->
        PListLiteral a t <$> desugarRecordPatterns ps
      p ->
        pure p

extractRow :: (HasType o k t) => t -> Row o k (Type o k)
extractRow e =
  case typeOf e of
    TIntrinsic (IRecord (TRow r)) ->
      r
    _ ->
      error "Implementation error"

extractVarName :: Maybe (Pattern a t) -> Name
extractVarName =
  \case
    Just (PVariable _ (Label _ name)) ->
      name
    _ ->
      "_"

desugar :: (Data a, Monoid a) => RecordInfo a -> Expression a IndexedType -> RecordDesugarStack a (Expression a IndexedType)
desugar (name, dict, zzz) expr = do
  names <- replicateM (length fields - 1) suppliedName
  let r1 = maybe RNil extractRow zzz
      v1 = extractVarName zzz
      t1 = maybe (TIntrinsic (IRecord (TRow RNil))) typeOf zzz
      e2 = ELet mempty (BPattern mempty (PVariable mempty (Label t1 v1)) (EVariable mempty (Label t1 (name <> ".tail"))) :| [])
  (_, _, e1) <- foldrM (go v1) (v1, r1, e2 expr) (zip fields (name : names))
  pure e1
 where
  fields = Map.toList dict

go :: (Data a, Monoid a) => Name -> ((Name, IndexedPattern a), Name) -> (Name, Row TypeIndex Kind IndexedType, Expression a IndexedType) -> RecordDesugarStack a (Name, Row TypeIndex Kind IndexedType, Expression a IndexedType)
go xx ((fname, p), prefix) (var, row, expr) = do
  let t1 = typeOf expr
      t2 = typeOf p
      ll1 = Label t2 (prefix <> ".field." <> fname)
      ll2 = Label (TIntrinsic (IRecord (TRow row))) (prefix <> ".tail")
      var1 = EVariable mempty (Label (TRow (RExtend fname t2 row)) prefix)
      var2 = EVariable mempty ll2

  e2 <-
    desugarRecordPatterns
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

  let e3 =
        EMatch
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

  let focusExpr =
        EFocus
          fname
          ll1
          ll2
          var1
          (if var == xx then e2 else e3)

  pure (prefix, RExtend fname t2 row, focusExpr)
