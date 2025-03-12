{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Noll.Core.Compiler.Pass.Memoize (memoize) where

import Control.Arrow ((>>>))
import Control.Monad.Writer (MonadWriter, tell)
import Data.Functor.Foldable (embed, project)
import Data.List (partition)
import Noll.Common.List1 (NonEmpty (..), fromList1)
import Noll.Core.Language (Binding (..), Expr, Type, bindingLabel, isFunction, isPrim)
import Noll.Utils ((<$$>))
import Noll.Utils.Operators ((||.))

import qualified Noll.Core.Language as Core

memoize :: (MonadWriter [Binding Type (Expr Type)] m) => Expr Type -> m (Expr Type)
memoize =
  project
    >>> \case
      Core.ELet vs e -> do
        let (ps, qs) = partition (not . (isFunction . bindingLabel ||. isPrim . bindingExpr)) (fromList1 vs)
        tell (Core.mem <$$> ps)
        case qs of
          u : us ->
            pure (Core.let_ (u :| us) e)
          [] ->
            pure e
      e ->
        pure (embed e)
