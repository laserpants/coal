{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Coal.LegacyKernel.Language.Expr.Replace (rewrite, Sub (..)) where

import Coal.Common.Label (Label (..), labelName, setLabelName)
import Coal.LegacyKernel.Language.Expr (Binding (..), Clause (..), Expr, ExprF (..), Focus (..), bindingLabel)
import qualified Coal.LegacyKernel.Language.Expr.Syntax as Syntax
import Control.Arrow ((>>>))
import Control.Monad.Identity (runIdentity)
import Data.Functor.Foldable (embed, para)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Extras (Dictionary, Map, Name, (<$$>))

replaceVarM :: (Monad m) => Name -> (Label t -> m (Expr t)) -> Expr t -> m (Expr t)
replaceVarM name fn =
  para $
    \case
      EVar ll@(Label _ var)
        | var == name -> fn ll
        | otherwise -> pure (Syntax.var ll)
      ELet vs e1
        | name `matchesAnyLabel` (bindingLabel <$> vs) -> do
            pure (Syntax.let_ (fst <$$> vs) (fst e1))
        | otherwise -> do
            ws <- traverse sequence (snd <$$> vs)
            Syntax.let_ ws <$> snd e1
      ELam vs e1
        | name `matchesAnyLabel` vs ->
            pure (Syntax.lam vs (fst e1))
        | otherwise ->
            Syntax.lam vs <$> snd e1
      ESel s@(Focus _ ll1 ll2) e1 e2
        | name `matchesAnyLabel` [ll1, ll2] ->
            pure (Syntax.sel s (fst e1) (fst e2))
        | otherwise ->
            Syntax.sel s
              <$> snd e1
              <*> snd e2
      EMat t e1 cs ->
        Syntax.match t
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

rewrite :: Name -> Name -> Expr t -> Expr t
rewrite old new = replaceVar old (Syntax.var . setLabelName new)

class Sub a where
  relabel :: Dictionary Name -> a -> a

instance (Sub s) => Sub [s] where
  relabel = fmap . relabel

instance (Sub s) => Sub (NonEmpty s) where
  relabel = fmap . relabel

instance (Sub s) => Sub (Map k s) where
  relabel = fmap . relabel

instance Sub (Expr t) where
  relabel dict expr = foldr (uncurry rewrite) expr (Map.toList dict)

{-# INLINE matchesLabel #-}
matchesLabel :: Name -> Label t -> Bool
matchesLabel name ll = labelName ll == name

{-# INLINE matchesAnyLabel #-}
matchesAnyLabel :: (Foldable f) => Name -> f (Label t) -> Bool
matchesAnyLabel = matchesLabel >>> any
