{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Coal.Kernel.Compiler.Pass.LetLifting (liftLetNodes) where

import Coal.Kernel.Language (Binding (..), Expr, Type, bindingLabel)
import qualified Coal.Kernel.Language as Syntax
import Coal.Kernel.Language.Type.Arrow (isFunction)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Functor.Foldable (cata, embed)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

liftLetNodes :: (MonadWriter [Binding Type (Expr Type)] m) => Expr Type -> m (Expr Type)
liftLetNodes =
  cata $
    \case
      Syntax.ELet vs e -> do
        as <- traverse sequence vs
        let (fs, es) = NonEmpty.partition (isFunction . bindingLabel) as
        tell fs
        case es of
          w : ws ->
            Syntax.let_ (w :| ws) <$> e
          [] ->
            e
      e ->
        embed <$> sequence e
