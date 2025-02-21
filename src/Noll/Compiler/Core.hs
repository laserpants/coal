{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Core where

import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Foldable (foldlM)
import Data.Functor.Foldable (cata, embed)
import Noll.Common.List1 (List1, NonEmpty (..), (<|))
import Noll.Common.Supply (supplied)
import Noll.Core.Language (Clause (..), Expr, Focus (..), Type, isFunction)
import Noll.Core.Language.Replace (relabeled, relabeled1, substVars)
import Noll.Label (Label (..))
import Noll.Utils (Dictionary, Name, isConstructor, (<$$>))
import TextShow

import qualified Data.Map.Strict as Map
import qualified Noll.Common.List1 as List1
import qualified Noll.Core.Language as Core

type Binding = (Label Type, Expr Type)

transLetLifting :: (MonadWriter [Binding] m) => Expr Type -> m (Expr Type)
transLetLifting =
  cata $
    \case
      Core.ELet vs e -> do
        as <- traverse sequence vs
        let (fs, es) = List1.partition (isFunction . fst) as
        tell fs
        case es of
          w : ws ->
            Core.let_ (w :| ws) <$> e
          [] ->
            e
      e ->
        embed <$> sequence e

transSuffixExpr :: (MonadState Int m) => Expr t -> m (Expr t)
transSuffixExpr =
  cata $
    \case
      Core.ELet vs e -> do
        let (lls, es) = List1.unzip vs
        sub <- mapping lls
        as <- sequence (substVars sub <$$> es)
        Core.let_ (List1.zip (relabeled sub lls) as) . substVars sub <$> e
      Core.ELam lls e -> do
        sub <- mapping lls
        Core.lam (relabeled sub lls) . substVars sub <$> e
      Core.ESel (Focus name ll2 ll3) e1 e2 -> do
        sub <- mapping (ll2 <| ll3 :| [])
        Core.sel (Focus name (relabeled1 sub ll2) (relabeled1 sub ll3))
          <$> e1
          <*> (substVars sub <$> e2)
      Core.EMat t e cs ->
        Core.match t
          <$> e
          <*> (traverse transSuffixClause =<< traverse sequence cs)
      e ->
        embed <$> sequence e

transSuffixClause :: (MonadState Int m) => Clause t (Expr t) -> m (Clause t (Expr t))
transSuffixClause =
  \case
    Clause lls e -> do
      sub <- mapping lls
      pure (Clause (relabeled sub lls) (substVars sub e))

mapping :: (MonadState Int m) => List1 (Label t) -> m (Dictionary Name)
mapping = foldlM go mempty
 where
  go dict (Label _ name) = do
    if isConstructor name
      then pure dict
      else do
        n <- supplied id
        pure (Map.insert name (name <> ".[" <> showt n <> "]") dict)
