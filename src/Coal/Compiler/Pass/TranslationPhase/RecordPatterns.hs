{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Coal.Compiler.Pass.TranslationPhase.RecordPatterns (passRecordPatterns) where

import Coal.Common.Label (Label (..))
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Journal (RecordInfo, listenRecordInfo, tellRecordInfo)
import Coal.Compiler.Pass
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Control.Monad.RWS (forM, replicateM)
import Data.Data (Data)
import Data.Foldable (foldrM)
import Data.Generics.Uniplate.Data (transformBiM)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Extra (Name)

passRecordPatterns :: (Monad m, Monoid a, Data a) => Pass a m (Module a Kind IndexedType) (Module a Kind IndexedType)
passRecordPatterns =
  Pass
    { passName = "RecordPatterns"
    , runPass = pass
    }

pass :: (Monad m, Monoid a, Data a) => Module a Kind IndexedType -> CompilerT a m (Module a Kind IndexedType)
pass = compileRecordPatterns

compileRecordPatterns :: forall m a. (Monad m, Data a, Monoid a) => Module a Kind IndexedType -> CompilerT a m (Module a Kind IndexedType)
compileRecordPatterns = transformBiM (desugarRecordPatterns @a @(Expression a (Type TypeIndex Kind)))

class RecordDesugarable a p where
  desugarRecordPatterns :: (Monad m) => p -> CompilerT a m p

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
        (q, fs) <- listenRecordInfo (desugarRecordPatterns p)
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
        name <- supplied (freshName "row")
        tellRecordInfo [(name, d, p)]
        pure (PConstructor mempty (Label t "$Record") [PVariable mempty (Label r name)])
      PListCons a t p1 p2 ->
        PListCons a t <$> desugarRecordPatterns p1 <*> desugarRecordPatterns p2
      PListLiteral a t ps ->
        PListLiteral a t <$> desugarRecordPatterns ps
      PTuple a t ps ->
        PTuple a t <$> desugarRecordPatterns ps
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

-- FIXME
desugar :: (Data a, Monoid a, Monad m) => RecordInfo a -> Expression a IndexedType -> CompilerT a m (Expression a IndexedType)
desugar (name, dict, p1) expr = do
  names <- replicateM (length fields - 1) (supplied (freshName "row"))
  let r1 = maybe RNil extractRow p1
      v1 = extractVarName p1
      t1 = maybe (TIntrinsic (IRecord (TRow RNil))) typeOf p1
      e2 = ELet mempty (BPattern mempty (PVariable mempty (Label t1 v1)) (EVariable mempty (Label t1 (name <> ".tail"))) :| [])
  (_, _, e1) <- foldrM (go v1) (v1, r1, e2 expr) (zip fields (name : names))
  pure e1
 where
  fields = Map.toList dict

-- FIXME
go :: (Data a, Monoid a, Monad m) => Name -> ((Name, IndexedPattern a), Name) -> (Name, Row TypeIndex Kind IndexedType, Expression a IndexedType) -> CompilerT a m (Name, Row TypeIndex Kind IndexedType, Expression a IndexedType)
go n ((fname, p), prefix) (var, row, expr) = do
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

  let focusExpr = EFocus fname ll1 ll2 var1 (if var == n then e2 else e3)

  pure (prefix, RExtend fname t2 row, focusExpr)
