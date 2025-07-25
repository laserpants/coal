{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Kernel.Compiler.Pass.Suffix (suffixExpr) where

import Control.Monad.State (MonadState, modify, runStateT)
import Control.Monad.Trans (lift)
import Data.Functor.Foldable (cata, embed)
import Extra (Dictionary, Name, applyM1, applyM2, isConstructor)
import Noll.Common.Label (Label (..))
import Noll.Common.List1 (List1, NonEmpty (..))
import Noll.Common.Supply (supplied)
import Noll.Kernel.Language (Binding (..), Clause (..), Expr, Focus (..), unzipBindings)
import Noll.Kernel.Language.Expr.Replace (Sub, relabel)
import TextShow (showt)

import qualified Data.Map.Strict as Map
import qualified Noll.Common.List1 as List1
import qualified Noll.Kernel.Language as Core

suffixExpr :: (MonadState Int m) => Expr t -> m (Expr t)
suffixExpr =
  cata $
    \case
      Core.ELet vs e -> do
        let (lls, es) = unzipBindings vs
        (lls1, a1, a2) <- applyM2 (addSuffix2 lls) (sequence es) e
        pure (Core.let_ (List1.zipWith Binding lls1 a1) a2)
      Core.ELam lls e -> do
        (lls1, a1) <- applyM1 (addSuffix lls) e
        pure (Core.lam lls1 a1)
      Core.ESel (Focus name ll2 ll3) e1 e2 -> do
        a1 <- e1
        (lls1, a2) <- applyM1 (addSuffix (ll2 :| [ll3])) e2
        case lls1 of
          (lls4 :| lls5 : _) ->
            pure (Core.sel (Focus name lls4 lls5) a1 a2)
          _ ->
            error "Implementation error"
      Core.EMat t e cs ->
        Core.match t
          <$> e
          <*> (traverse suffixClause =<< traverse sequence cs)
      e ->
        embed <$> sequence e

suffixClause :: (MonadState Int m) => Clause t (Expr t) -> m (Clause t (Expr t))
suffixClause =
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
