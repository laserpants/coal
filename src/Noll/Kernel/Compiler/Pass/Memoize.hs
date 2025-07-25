{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Noll.Kernel.Compiler.Pass.Memoize (memoize) where

import Control.Arrow ((>>>))
import Control.Monad.Writer (MonadWriter, tell)
import Data.Functor.Foldable (embed, project)
import Data.List (partition)
import Noll.Common.List1 (NonEmpty (..), fromList1)
import Noll.Common.FreeVars (freeSet)
import Noll.Common.Label (Label (..))
import Noll.Kernel.Language (Binding (..), Expr, Type, bindingLabel, isFunction, isPrim)
import Extra (Set, (<$$>))

import qualified Noll.Kernel.Language as Core

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
      Core.ELet vs e -> do
        let (ps, qs) = partition canMemo (fromList1 vs)
        tell (Core.mem <$$> ps)
        case qs of
          u : us ->
            pure (Core.let_ (u :| us) e)
          [] ->
            pure e
      e ->
        pure (embed e)
