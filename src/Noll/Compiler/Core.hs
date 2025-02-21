{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Core where

import Control.Monad.State (MonadState, modify, runStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Functor.Foldable (cata, embed)
import Noll.Common.List1 (List1, NonEmpty (..), (<|))
import Noll.Common.Supply (supplied)
import Noll.Core.Language (Clause (..), Expr, Focus (..), Type, isFunction)
import Noll.Core.Language.Replace (Sub, relabel)
import Noll.Label (Label (..))
import Noll.Utils (Dictionary, Name, isConstructor)
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
        a <- e
        as <- sequence es
        (lls1, a1, a2) <- addSuffix2 lls as a
        pure (Core.let_ (List1.zip lls1 a1) a2)
      Core.ELam lls e -> do
        a <- e
        (lls1, a1) <- addSuffix lls a
        pure (Core.lam lls1 a1)
      Core.ESel (Focus name ll2 ll3) e1 e2 -> do
        a1 <- e1
        (lls1, a2) <- addSuffix (ll2 <| ll3 :| []) =<< e2
        case lls1 of
          (lls4 :| lls5 : _) ->
            pure (Core.sel (Focus name lls4 lls5) a1 a2)
          _ ->
            error "Implementation error"
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
      (lls1, a) <- addSuffix lls e
      pure (Clause lls1 a)

addSuffix :: (MonadState Int m, Sub s) => List1 (Label t) -> s -> m (List1 (Label t), s)
addSuffix lls e = do
  (lls1, sub) <- mapping lls
  pure (lls1, relabel sub e)

addSuffix2 :: (MonadState Int m, Sub s1, Sub s2) => List1 (Label t) -> s1 -> s2 -> m (List1 (Label t), s1, s2)
addSuffix2 lls e1 e2 = do
  (lls1, sub) <- mapping lls
  pure (lls1, relabel sub e1, relabel sub e2)

mapping :: (MonadState Int m) => List1 (Label t) -> m (List1 (Label t), Dictionary Name)
mapping lls = runStateT (traverse go lls) mempty
 where
  go ll@(Label t name)
    | isConstructor name =
        pure ll
    | otherwise = do
        n <- lift (supplied id)
        let name1 = name <> ".[" <> showt n <> "]"
        modify (Map.insert name name1)
        pure (Label t name1)
