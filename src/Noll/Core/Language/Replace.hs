{-# LANGUAGE LambdaCase #-}

module Noll.Core.Language.Replace (relabeled, relabeled1, substVar, substVars) where

import Control.Arrow ((>>>))
import Control.Monad.Identity (runIdentity)
import Data.Functor.Foldable (embed, para)
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (second)
import Noll.Common.List1 (List1)
import Noll.Core.Language (Clause (..), Expr, ExprF (..), Focus (..))
import Noll.Label (Label (..), labelName, setLabelName)
import Noll.Utils (Dictionary, Name)

import qualified Data.Map.Strict as Map
import qualified Noll.Core.Language as Core

replaceVarM :: (Monad m) => Name -> (Label t -> m (Expr t)) -> Expr t -> m (Expr t)
replaceVarM name fn =
  para $
    \case
      EVar ll@(Label _ var)
        | var == name -> fn ll
        | otherwise -> pure (Core.var ll)
      ELet vs e1
        | name `matchesAnyLabel` (fst <$> vs) ->
            pure (Core.let_ (second fst <$> vs) (fst e1))
        | otherwise ->
            Core.let_
              <$> traverse (sequence <$> second snd) vs
              <*> snd e1
      ELam vs e1
        | name `matchesAnyLabel` vs ->
            pure (Core.lam vs (fst e1))
        | otherwise ->
            Core.lam vs <$> snd e1
      ESel s@(Focus _ ll1 ll2) e1 e2
        | name `matchesAnyLabel` [ll1, ll2] ->
            pure (Core.sel s (fst e1) (fst e2))
        | otherwise ->
            Core.sel s
              <$> snd e1
              <*> snd e2
      EMat t e1 cs ->
        Core.match t
          <$> snd e1
          <*> traverse modClause cs
      e ->
        embed <$> mapM snd e
 where
  modClause (Clause lls e)
    | name `matchesAnyLabel` lls =
        pure (Clause lls (fst e))
    | otherwise =
        Clause lls <$> snd e

replaceVar :: Name -> (Label t -> Expr t) -> Expr t -> Expr t
replaceVar name fn = runIdentity . replaceVarM name (pure . fn)

{-# INLINE relabeled #-}
relabeled :: Dictionary Name -> List1 (Label t) -> List1 (Label t)
relabeled = fmap . relabeled1

relabeled1 :: Dictionary Name -> Label t -> Label t
relabeled1 dict (Label t name) = Label t (fromMaybe name (Map.lookup name dict))

substVars :: Dictionary Name -> Expr t -> Expr t
substVars dict expr = foldr (uncurry substVar) expr (Map.toList dict)

substVar :: Name -> Name -> Expr t -> Expr t
substVar old new = replaceVar old (Core.var . setLabelName new)

{-# INLINE matchesLabel #-}
matchesLabel :: Name -> Label t -> Bool
matchesLabel name ll = labelName ll == name

{-# INLINE matchesAnyLabel #-}
matchesAnyLabel :: (Foldable f) => Name -> f (Label t) -> Bool
matchesAnyLabel = matchesLabel >>> any
