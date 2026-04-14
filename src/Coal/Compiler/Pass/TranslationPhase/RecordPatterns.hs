{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TranslationPhase.RecordPatterns (
  passRecordPatterns,
) where

import Coal.Common.Label (Label (..))
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Journal (RecordEntry, listenRecordEntry, tellRecordEntry)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Control.Monad.RWS (forM, replicateM)
import Data.Data (Data)
import Data.Foldable (foldrM)
import Data.Generics.Uniplate.Data (transformBiM)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Extras (Name)

passRecordPatterns :: (Monad m, Monoid a, Data a) => Pass a m (Module a Kind IndexedType) (Module a Kind IndexedType)
passRecordPatterns = Pass{runPass = passImpl}

passImpl :: (Monad m, Data a, Monoid a) => Module a Kind IndexedType -> CompilerT a m (Module a Kind IndexedType)
passImpl = transformBiM go
 where
  go :: (Monad m, Data a, Monoid a) => Expression a Kind IndexedType -> CompilerT a m (Expression a Kind IndexedType)
  go = desugarRecordPatterns

class RecordContext a p where
  desugarRecordPatterns :: (Monad m) => p -> CompilerT a m p

instance (RecordContext a p) => RecordContext a (Maybe p) where
  desugarRecordPatterns = traverse desugarRecordPatterns

instance (RecordContext a p) => RecordContext a [p] where
  desugarRecordPatterns = traverse desugarRecordPatterns

instance (RecordContext a p) => RecordContext a (NonEmpty p) where
  desugarRecordPatterns = traverse desugarRecordPatterns

instance (Data a, Monoid a) => RecordContext a (Expression a Kind IndexedType) where
  desugarRecordPatterns =
    \case
      EMatch a t e ks -> do
        es <- forM (NonEmpty.init $ NonEmpty.tails ks) $
          \case
            (EClause a1 p cs : rest) -> do
              (q, fs) <- listenRecordEntry (desugarRecordPatterns p)
              cs' <- forM cs $
                \case
                  CPlain a2 gs e1 -> do
                    e2 <- desugarRecordPatterns e1
                    CPlain a2 <$> desugarRecordPatterns gs <*> foldrM (desugar t e rest) e2 fs
              pure (EClause a1 q cs')
            _ ->
              error "Not implemented"
        pure (EMatch a t e (NonEmpty.fromList es))
      e ->
        pure e

instance (Data a, Monoid a) => RecordContext a (Guard Expression a Kind IndexedType) where
  desugarRecordPatterns =
    \case
      CGuard e ->
        CGuard <$> desugarRecordPatterns e

desugarShorthandPatterns :: Pattern a Kind IndexedType -> Pattern a Kind IndexedType
desugarShorthandPatterns =
  \case
    PShorthand loc (Label t name) ->
      PVariable loc (Label t name)
    p ->
      p

instance (Data a, Monoid a) => RecordContext a (Pattern a Kind IndexedType) where
  desugarRecordPatterns =
    \case
      PAnnotation a t p ->
        PAnnotation a t <$> desugarRecordPatterns p
      PConstructor a ll ps ->
        PConstructor a ll <$> desugarRecordPatterns ps
      PRecord _ t@(TRecord r) d p -> do
        name <- supplied (freshName "row")
        tellRecordEntry [(name, fmap desugarShorthandPatterns d, p)]
        pure (PConstructor mempty (Label t "$Record") [PVariable mempty (Label r name)])
      PListCons a t p1 p2 ->
        PListCons a t <$> desugarRecordPatterns p1 <*> desugarRecordPatterns p2
      PListLiteral a t ps ->
        PListLiteral a t <$> desugarRecordPatterns ps
      PTuple a t ps ->
        PTuple a t <$> desugarRecordPatterns ps
      PAs a ll p ->
        PAs a ll <$> desugarRecordPatterns p
      p ->
        pure p

extractRow :: (HasType o k t) => t -> Row o k (Type o k)
extractRow e =
  case typeOf e of
    TRecord (TRow r) ->
      r
    _ ->
      error "Implementation error"

extractVarName :: Maybe (Pattern a k t) -> Name
extractVarName =
  \case
    Just (PVariable _ (Label _ name)) ->
      name
    _ ->
      "_"

desugar :: (Data a, Monoid a, Monad m) => IndexedType -> Expression a Kind IndexedType -> [Clause a Kind IndexedType] -> RecordEntry a -> Expression a Kind IndexedType -> CompilerT a m (Expression a Kind IndexedType)
desugar t0 e0 rest (name, dict, p1) expr = do
  names <- replicateM (length fields - 1) (supplied (freshName "row"))
  (_, _, e1) <- foldrM go (v1, r1, e2 expr) (zip fields (name : names))
  pure e1
 where
  fields = Map.toList dict
  r1 = maybe RNil extractRow p1
  v1 = extractVarName p1
  t1 = maybe (TRecord (TRow RNil)) typeOf p1
  e2 = ELet mempty (BPattern mempty (PVariable mempty (Label t1 v1)) (EVariable mempty (Label t1 (name <> ".tail"))) :| [])
  go ((fname, p), prefix) (var, row, expr2) = do
    let t2 = typeOf p
        ll1 = Label (typeOf p) (prefix <> ".field." <> fname)
        ll2 = Label (TRecord (TRow row)) (prefix <> ".tail")
        match = EMatch mempty (typeOf expr2)
        clause q e = EClause mempty q (CPlain mempty [] e :| [])
        focus = EFocus mempty fname ll1 ll2 (EVariable mempty (Label (TRow (RExtend fname t2 row)) prefix))
    e3 <-
      desugarRecordPatterns
        ( match
            (EVariable mempty ll1)
            ( clause p expr2
                :| ( case rest of
                      [] -> []
                      q : qs ->
                        [ EClause
                            mempty
                            (PAny mempty t2)
                            ( CPlain
                                mempty
                                []
                                (EMatch mempty t0 e0 (q :| qs))
                                :| []
                            )
                        ]
                   )
            )
        )
    pure
      ( prefix
      , RExtend fname t2 row
      , focus
          ( if var == v1
              then e3
              else
                match
                  (EVariable mempty ll2)
                  ( clause
                      ( PConstructor
                          mempty
                          (Label (TRecord (TRow row)) "$Record")
                          [PVariable mempty (Label (TRow row) var)]
                      )
                      e3
                      :| []
                  )
          )
      )
