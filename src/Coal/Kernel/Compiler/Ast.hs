{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Coal.Kernel.Compiler.Ast (
  flattenLambdaNodes,
  flattenAppNodes,
  flattenObject,
  simplifyLetNodes,
  sortMatchClauses,
) where

import Coal.Common.Label (Label (..))
import Coal.Common.List1 (NonEmpty (..), fromList1)
import Coal.Kernel.Language
import Control.Monad.Writer (runWriter, tell)
import Data.Fix (Fix (..))
import Data.Function (on)
import Data.Functor.Foldable (cata, embed)
import Extra (foldrM)

import qualified Coal.Common.List1 as List1
import qualified Data.Map.Strict as Map

flattenObject :: Object Type (Expr Type) -> Object Type (Expr Type)
flattenObject =
  \case
    OFunction name lls1 (Fix (ELam lls2 e)) ->
      OFunction name (lls1 <> fromList1 lls2) e
    OConstant name (Fix (ELam lls e)) ->
      OFunction name (fromList1 lls) e
    o ->
      o

flattenLambdaNodes :: Expr Type -> Expr Type
flattenLambdaNodes =
  cata $
    \case
      ELam vs1 (Fix (ELam vs2 e1)) ->
        lam (vs1 <> vs2) e1
      e ->
        embed e

flattenAppNodes :: Expr t -> Expr t
flattenAppNodes =
  cata $
    \case
      EApp t (Fix (EApp _ e1 es1)) es2 ->
        app t e1 (es1 <> es2)
      e ->
        embed e

simplifyLetNodes :: Expr t -> Expr t
simplifyLetNodes e = relabel (Map.fromList sub) e1
 where
  subst =
    cata $
      \case
        ELet vs f -> do
          binds <- foldrM go [] =<< traverse sequence vs
          case binds of
            a : as ->
              let_ (a :| as) <$> f
            [] ->
              f
        f ->
          embed <$> sequence f

  go (Binding ll1 (Fix (EVar ll2))) ls = do
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
        match t e1 (List1.sortBy clauseOrder cs)
      e ->
        embed e
 where
  clauseOrder (Clause (a :| _) _) (Clause (b :| _) _) =
    (compare `on` labelName) a b
