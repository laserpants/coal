{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Compiler.Pass.LambdaLifting (liftLambdaNodes) where

import Coal.Common.Label (Label (..))
import Coal.Common.List1 (List1, fromList1)
import Coal.Common.Supply (supplied)
import Coal.Kernel.Language (Binding (..), Expr, Type, functionTypeOf)
import Coal.Kernel.Language.Object (Object (..), ObjectList)
import Control.Monad.RWS (MonadWriter, RWS, ask, evalRWS, local, tell)
import Data.Functor.Foldable (cata, embed)
import Extra (Name, forM, traverse2)
import TextShow (showt)

import qualified Coal.Kernel.Language as Core
import qualified Data.Text as Text

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
        Core.ELet vs e -> do
          ws <- forM vs $
            \(Binding ll@(Label _ name) e1) -> do
              f <- local (const name) e1
              pure (Binding ll f)
          f <- local mempty e
          pure (Core.let_ ws f)
        Core.ELam vs e -> do
          n <- supplied id
          name <- ask
          f <- local mempty e
          moveUp (if Text.null name then "$anonymous_fn." <> showt n else name) vs f
        e ->
          local mempty (embed <$> sequence e)

moveUp :: (MonadWriter ObjectList m) => Name -> List1 (Label Type) -> Expr Type -> m (Expr Type)
moveUp name vs f = do
  tell [OFunction name (fromList1 vs) f]
  pure (Core.var (Label (functionTypeOf f vs) name))
