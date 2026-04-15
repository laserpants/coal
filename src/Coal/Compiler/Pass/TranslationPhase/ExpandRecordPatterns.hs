{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TranslationPhase.ExpandRecordPatterns (
  passExpandRecordPatterns,
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

passExpandRecordPatterns :: (Monad m, Monoid a, Data a) => Pass a m (Module a Kind IndexedType) (Module a Kind IndexedType)
passExpandRecordPatterns = Pass{runPass = transformBiM passImpl}

passImpl :: (Monad m, Data a, Monoid a) => Expression a Kind IndexedType -> CompilerT a m (Expression a Kind IndexedType)
passImpl = desugarRecordPatterns

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
  (_, _, result) <- foldrM processField (varName, initialRow, initialExpr expr) (zip fields (name : names))
  pure result
 where
  fields = Map.toList dict
  initialRow = maybe RNil extractRow p1
  varName = extractVarName p1
  rowType = maybe (TRecord (TRow RNil)) typeOf p1

  initialExpr =
    ELet
      mempty
      ( BPattern
          mempty
          (PVariable mempty (Label rowType varName))
          (EVariable mempty (Label rowType (name <> ".tail")))
          :| []
      )

  processField ((fieldName, pattern), prefix) (currentVar, currentRow, currentExpr) = do
    let fieldType = typeOf pattern
        fieldLabel = Label fieldType (prefix <> ".field." <> fieldName)
        tailLabel = Label (TRecord (TRow currentRow)) (prefix <> ".tail")
        extendedRowType = TRow (RExtend fieldName fieldType currentRow)

        makeClause pat body = EClause mempty pat (CPlain mempty [] body :| [])

        fieldFocus =
          EFocus
            mempty
            fieldName
            fieldLabel
            tailLabel
            (EVariable mempty (Label extendedRowType prefix))

        fallbackClauses =
          case rest of
            [] -> []
            q : qs ->
              [ EClause
                  mempty
                  (PAny mempty fieldType)
                  (CPlain mempty [] (EMatch mempty t0 e0 (q :| qs)) :| [])
              ]

    matchedExpr <-
      desugarRecordPatterns $
        EMatch
          mempty
          (typeOf currentExpr)
          (EVariable mempty fieldLabel)
          (makeClause pattern currentExpr :| fallbackClauses)

    let wrappedExpr =
          if currentVar == varName
            then matchedExpr
            else
              EMatch
                mempty
                (typeOf matchedExpr)
                (EVariable mempty tailLabel)
                ( makeClause
                    ( PConstructor
                        mempty
                        (Label (TRecord (TRow currentRow)) "$Record")
                        [PVariable mempty (Label (TRow currentRow) currentVar)]
                    )
                    matchedExpr
                    :| []
                )

    return (prefix, RExtend fieldName fieldType currentRow, fieldFocus wrappedExpr)
