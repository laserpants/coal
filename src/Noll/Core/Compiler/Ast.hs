{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Noll.Core.Compiler.Ast (
  flattenELam,
  flattenEApp,
  simplifyELet,
  sortMatchClauses,
) where

import Control.Monad.Writer (MonadWriter, runWriter, tell)
import Data.Fix (Fix (..))
import Data.Functor.Foldable (cata, embed, project)
import Noll.Common.List1 (NonEmpty (..))
import Noll.Core.Language (Binding (..), Clause (..), Expr, ExprF (..), Type)
import Noll.Core.Language.Expr.Replace (relabel)
import Noll.Label (Label (..))
import Noll.Utils (foldrM)

import qualified Data.Map.Strict as Map
import qualified Noll.Common.List1 as List1
import qualified Noll.Core.Language as Core

flattenELam :: Expr Type -> Expr Type
flattenELam =
  cata $
    \case
      Core.ELam vs1 (Fix (Core.ELam vs2 e1)) ->
        Core.lam (vs1 <> vs2) e1
      e ->
        embed e

flattenEApp :: Expr t -> Expr t
flattenEApp =
  cata $
    \case
      Core.EApp t (Fix (Core.EApp _ e1 es1)) es2 ->
        Core.app t e1 (es1 <> es2)
      e ->
        embed e

simplifyELet :: Expr t -> Expr t
simplifyELet e = relabel (Map.fromList sub) e1
 where
  subst =
    cata $
      \case
        ELet vs f -> do
          binds <- foldrM go [] =<< traverse sequence vs
          case binds of
            a : as ->
              Core.let_ (a :| as) <$> f
            [] ->
              f
        f ->
          embed <$> sequence f

  go (Binding ll1 (Fix (Core.EVar ll2))) ls = do
    tell [(labelName ll1, labelName ll2)]
    pure ls
  go l ls =
    pure (l : ls)
  (e1, sub) =
    runWriter (subst e)

sortMatchClauses :: Expr t -> Expr t
sortMatchClauses =
  cata $
    \case
      EMat t e1 cs ->
        Core.match t e1 (List1.sortBy clauseOrder cs)
      e ->
        embed e
 where
  clauseOrder (Clause (a :| _) _) (Clause (b :| _) _) =
    compare (labelName a) (labelName b)
