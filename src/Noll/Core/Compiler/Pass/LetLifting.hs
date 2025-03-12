{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Noll.Core.Compiler.Pass.LetLifting (transformLetLifting) where

import Control.Monad.Writer (MonadWriter, tell)
import Data.Functor.Foldable (cata, embed)
import Noll.Common.List1 (NonEmpty (..))
import Noll.Core.Language (Binding (..), Expr, Type, bindingLabel)
import Noll.Core.Language.Type.Arrow (isFunction)

import qualified Noll.Common.List1 as List1
import qualified Noll.Core.Language as Core

transformLetLifting :: (MonadWriter [Binding Type (Expr Type)] m) => Expr Type -> m (Expr Type)
transformLetLifting =
  cata $
    \case
      Core.ELet vs e -> do
        as <- traverse sequence vs
        let (fs, es) = List1.partition (isFunction . bindingLabel) as
        tell fs
        case es of
          w : ws ->
            Core.let_ (w :| ws) <$> e
          [] ->
            e
      e ->
        embed <$> sequence e
