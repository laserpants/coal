{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.LegacyKernel.Compiler.Pass.LambdaLifting (liftLambdaNodes) where

import Coal.Common.Label (Label (..))
import Coal.Common.Supply (supplied)
import Coal.LegacyKernel.Language
import Control.Monad.RWS (MonadWriter, RWS, ask, evalRWS, local, tell)
import Data.Functor.Foldable (cata, embed)
import Data.List.NonEmpty (NonEmpty, toList)
import qualified Data.Text as Text
import Extras (Name, forM, traverse2)
import TextShow (showt)

runLifting :: RWS Name ObjectList Int a -> (a, ObjectList)
runLifting e = evalRWS e "" 1

liftLambdaNodes :: ObjectList -> ObjectList
liftLambdaNodes objs = objs1 <> objs2
 where
  (objs1, objs2) =
    runLifting (traverse2 go objs)
  go =
    cata $
      \case
        ELet vs e -> do
          ws <- forM vs $
            \(Binding ll@(Label _ name) e1) -> do
              f <- local (const name) e1
              pure (Binding ll f)
          f <- local mempty e
          pure (let_ ws f)
        ELam vs e -> do
          n <- supplied id
          name <- ask
          f <- local mempty e
          moveUp (if Text.null name then "$anonymous_fn." <> showt n else name) vs f
        e ->
          local mempty (embed <$> sequence e)

moveUp :: (MonadWriter ObjectList m) => Name -> NonEmpty (Label Type) -> Expr Type -> m (Expr Type)
moveUp name vs f = do
  tell [OFunction name (toList vs) f]
  pure (var (Label (functionTypeOf f vs) name))
