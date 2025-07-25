{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Coal.Kernel.Compiler.Pass.LetLifting (liftLetNodes) where

import Coal.Common.List1 (NonEmpty (..))
import Coal.Kernel.Language (Binding (..), Expr, Type, bindingLabel)
import Coal.Kernel.Language.Type.Arrow (isFunction)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Functor.Foldable (cata, embed)

import qualified Coal.Common.List1 as List1
import qualified Coal.Kernel.Language as Core

liftLetNodes :: (MonadWriter [Binding Type (Expr Type)] m) => Expr Type -> m (Expr Type)
liftLetNodes =
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
