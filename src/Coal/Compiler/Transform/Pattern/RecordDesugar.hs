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

module Coal.Compiler.Transform.Pattern.RecordDesugar (
  BorkStack (..),
  Borkable (..),
  borkRecordPatterns,
  evalBorkStack,
  runBorkStack,
) where

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

borkRecordPatterns :: forall a. (Data a, Monoid a) => Module a Kind IndexedType -> BorkStack a (Module a Kind IndexedType)
borkRecordPatterns = transformBiM (bork @a @(Expression a (Type TypeIndex Kind)))

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

class Borkable a p where
  bork :: p -> BorkStack a p

instance (Borkable a p) => Borkable a (Maybe p) where
  bork = traverse bork

instance (Borkable a p) => Borkable a [p] where
  bork = traverse bork

instance (Borkable a p) => Borkable a (NonEmpty p) where
  bork = traverse bork

instance (Data a, Monoid a) => Borkable a (Expression a IndexedType) where
  bork =
    \case
      EMatch a t e cs ->
        EMatch a t e <$> bork cs
      EFold a t es cs e ->
        EFold a t es cs <$> bork e
      EUnfold a t ll n ps d me ->
        EUnfold a t ll n ps d <$> bork me
      e ->
        pure e

instance (Data a, Monoid a) => Borkable a (Guard Expression a IndexedType) where
  bork =
    \case
      CGuard e ->
        CGuard <$> bork e

instance (Data a, Monoid a) => Borkable a (Clause a IndexedType) where
  bork =
    \case
      EClause a p cs -> do
        (q, fs) <- listen (bork p)
        ds <- forM cs $
          \case
            CPlain a1 gs e -> do
              hs <- bork gs
              e1 <- foldrM borkbork e fs
              pure (CPlain a1 hs e1)
            CLambda{} ->
              error "Not implemented"
        pure (EClause a q ds)

instance (Data a, Monoid a) => Borkable a (Pattern a IndexedType) where
  bork =
    \case
      PAnnotation a t p ->
        PAnnotation a t <$> bork p
      PConstructor a ll ps ->
        PConstructor a ll <$> bork ps
      PRecord _ t@(TIntrinsic (IRecord r)) d p -> do
        name <- suppliedName
        tell [(name, d, p)]
        pure (PConstructor mempty (Label t "$Record") [PVariable mempty (Label r name)])
      PListCons a t p1 p2 ->
        PListCons a t <$> bork p1 <*> bork p2
      PListLiteral a t ps ->
        PListLiteral a t <$> bork ps
      p ->
        pure p

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
    bork
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
