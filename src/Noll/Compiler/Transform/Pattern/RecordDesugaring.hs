{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Pattern.RecordDesugaring where

import Control.Monad.RWS
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Control.Monad.Writer
import Data.Data (Data, Typeable)
import Data.Foldable (foldlM, foldrM)
import Debug.Trace
import Lang.Common.List1 (List1, NonEmpty (..))
import Lang.Common.Supply (suppliedName)
import Lang.Label (Label (..))
import Lang.Utils (Dictionary, Map, Name, forM_, traverseM)
import Noll.Ast.HasType (HasType (..))
import Noll.Language
import Noll.Language.Type.Row (RowData (..))

import qualified Data.Map.Strict as Map

type TypedPattern a = Pattern a (Type TypeIndex Kind)

class RecordPattern a p where
  expandRecordPatterns :: (MonadState Int m, MonadWriter [(Name, Dictionary (TypedPattern a), Maybe (TypedPattern a))] m, MonadReader Name m) => p -> m p

-- runExpandRecordPatterns :: (MonadState Int m, MonadWriter [(Name, Dictionary (TypedPattern a), Maybe (TypedPattern a))] m, MonadReader Name m) => m a -> a
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

-- instance (Monoid a, Show a) => RecordPattern a (Choice Expression a (Type TypeIndex Kind)) where
--  expandRecordPatterns =
--    \case
--      CPlain a gs e ->
--        CPlain a <$> expandRecordPatterns gs <*> expandRecordPatterns e
--      CLambda a ps gs e ->
--        CLambda a <$> expandRecordPatterns ps <*> expandRecordPatterns gs <*> expandRecordPatterns e

instance (Data a, Monoid a, Show a) => RecordPattern a (Clause a (Type TypeIndex Kind)) where
  expandRecordPatterns =
    \case
      EClause a p cs -> do
        -- let zz = p :: Int
        (q, ys :: [(Name, Dictionary (TypedPattern a), Maybe (TypedPattern a))]) <- runWriterT (expandRecordPatterns p)
        -- traceShowM "****************"
        -- traceShowM y
        -- EClause a x <$> expandRecordPatterns cs
        ds <- forM cs $
          \case
            CPlain a gs e -> do
              hs <- expandRecordPatterns gs
              f <- foldrM zork e ys
              pure (CPlain a hs f)
            CLambda a ps gs e ->
              undefined
        pure (EClause a q ds)

zork ::
  (Data a, Show a, Monoid a, MonadState Int m, MonadWriter [(Name, Dictionary (TypedPattern a), Maybe (TypedPattern a))] m, MonadReader Name m) =>
  (Name, Dictionary (TypedPattern a), Maybe (TypedPattern a)) ->
  Expression a (Type TypeIndex Kind) ->
  m (Expression a (Type TypeIndex Kind))
zork (name, d, mp) e = do
  --  traceShowM "<<<<<<<<<<<<<"
  --  traceShowM "<<<<<<<<<<<<<"
  --  traceShowM "<<<<<<<<<<<<<"
  names <- replicateM (length zz - 1) suppliedName
  -- traceShowM (zip zz (reverse names))
  --  traceShowM (zip zz ((name : names)))
  --  traceShowM "<<<<<<<<<<<<<"
  --  traceShowM "<<<<<<<<<<<<<"
  --  traceShowM "<<<<<<<<<<<<<"

  -- [ (("max",PVariable () (Label {labelTag = TVariable (TypeIndex {typeIndexKind = KType, typeIndexId = 0}), labelName = "max"})),"$baz.2")
  -- , (("min",PVariable () (Label {labelTag = TVariable (TypeIndex {typeIndexKind = KType, typeIndexId = 0}), labelName = "min"})),"$baz.3")
  -- ]

  --  traceShowM "**************"
  --  traceShowM tttr
  --  traceShowM "**************"

  -- aa <- foldrM tork (name, e) zz
  (_, _, aa) <- foldrM tork ("_", RNil, e) (zip zz (name : names))

  pure aa
 where
  zz = Map.toList d

--  yy =
--    case mp of
--      Nothing ->
--        undefined
--      Just xx ->
--        undefined

-- tork :: (Monad m, Monoid a, MonadState Int m, MonadReader Name m) => (Name, (TypedPattern a)) -> (Name, Expression a (Type TypeIndex Kind)) -> m (Name, Expression a (Type TypeIndex Kind))
-- tork :: (Monad m, Monoid a, MonadState Int m, MonadReader Name m) =>
--          (Name, Expression a (Type TypeIndex Kind)) ->
--          (Name, (TypedPattern a)) ->
--          m (Name, Expression a (Type TypeIndex Kind))
tork ((name, p), rrr) (x, tttr, e) = do
  --  traceShowM ("rrr", rrr)
  --  traceShowM ("p", p)
  --  traceShowM ("tp", typeOf p :: Type TypeIndex Kind)
  --  traceShowM ("e", e)
  --  traceShowM ("x", x)
  --  traceShowM ("t", typeOf e :: Type TypeIndex Kind)
  --  traceShowM ("x", x)
  --  traceShowM ("t", typeOf e :: Type TypeIndex Kind)

  -- tork :: (Monad m, Monoid a, MonadState Int m, MonadReader Name m) => (Name, (TypedPattern a)) -> (Name, Expression a (Type TypeIndex Kind)) -> m (Name, Expression a (Type TypeIndex Kind))
  -- tork (name, p) (x, e) = do
  --  zzz <- suppliedName
  pure $
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

-- pure (EClause a x cs)

instance (Monoid a, Show a) => RecordPattern a (Pattern a (Type TypeIndex Kind)) where
  expandRecordPatterns =
    \case
      PAnnotation a t p ->
        PAnnotation a t <$> expandRecordPatterns p
      PConstructor a ll ps ->
        PConstructor a ll <$> traverse expandRecordPatterns ps
      p@PVariable{} ->
        pure p
      PRecord _ t@(TIntrinsic (IRecord r)) d p -> do
        name <- suppliedName
        tell [(name, d, p)]
        pure (PConstructor mempty (Label t "$Record") [PVariable mempty (Label r name)])
      p -> do
        error "TODO"

--        traceShowM "****************"
--        traceShowM p
--        pure p
