{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Coal.Kernel.Compiler.Pass.Memoize (memoize) where

import Coal.Common.FreeVars (freeSet)
import Coal.Common.Label (Label (..))
import Coal.Kernel.Language (Binding (..), Expr, Type, bindingLabel, isFunction, isPrim)
import Control.Arrow ((>>>))
import Control.Monad.Writer (MonadWriter, tell)
import Data.Functor.Foldable (embed, project)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Extra (Set, (<$$>))

import qualified Coal.Kernel.Language as Syntax

canMemo :: Binding Type (Expr Type) -> Bool
canMemo b
  | isFunction (bindingLabel b) = False
  | isPrim (bindingExpr b) = False
  | not (null freeVars) = False
  | otherwise = True
 where
  freeVars :: Set (Label Type)
  freeVars = freeSet [] (bindingExpr b)

memoize :: (MonadWriter [Binding Type (Expr Type)] m) => Expr Type -> m (Expr Type)
memoize =
  project
    >>> \case
      Syntax.ELet vs e -> do
        let (ps, qs) = partition canMemo (toList vs)
        tell (Syntax.mem <$$> ps)
        case qs of
          u : us ->
            pure (Syntax.let_ (u :| us) e)
          [] ->
            pure e
      e ->
        pure (embed e)
