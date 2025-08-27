{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Compiler.Pass.Suffix (suffixExpr) where

import Coal.Common.Label (Label (..))
import Coal.Common.List1 (List1, NonEmpty (..))
import Coal.Common.Supply (supplied)
import Coal.Kernel.Language
import Control.Monad.State (MonadState, modify, runStateT)
import Control.Monad.Trans (lift)
import Data.Functor.Foldable (cata, embed)
import Extra (Dictionary, Name, applyM1, applyM2, isConstructor)
import TextShow (showt)

import qualified Coal.Common.List1 as List1
import qualified Data.Map.Strict as Map

suffixExpr :: (MonadState Int m) => Expr t -> m (Expr t)
suffixExpr =
  cata $
    \case
      ELet vs e -> do
        let (lls, es) = unzipBindings vs
        (lls1, a1, a2) <- applyM2 (addSuffix2 lls) (sequence es) e
        pure (let_ (List1.zipWith Binding lls1 a1) a2)
      ELam lls e -> do
        (lls1, a1) <- applyM1 (addSuffix lls) e
        pure (lam lls1 a1)
      ESel (Focus name ll2 ll3) e1 e2 -> do
        a1 <- e1
        (lls1, a2) <- applyM1 (addSuffix (ll2 :| [ll3])) e2
        case lls1 of
          (lls4 :| lls5 : _) ->
            pure (sel (Focus name lls4 lls5) a1 a2)
          _ ->
            error "Implementation error"
      EMat t e cs ->
        match t
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
        let name' = name <> ".[" <> showt n <> "]"
        modify (Map.insert name name')
        pure (Label t name')
