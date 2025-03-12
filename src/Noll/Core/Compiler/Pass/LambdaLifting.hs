{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.Compiler.Pass.LambdaLifting (liftLambdas) where

import Control.Monad.RWS (MonadWriter, RWS, ask, evalRWS, local, tell)
import Data.Functor.Foldable (cata, embed)
import Noll.Common.List1 (List1, fromList1)
import Noll.Common.Supply (supplied)
import Noll.Core.Language (Binding (..), Expr, Type, functionTypeOf)
import Noll.Core.Language.Object (Object (..), ObjectList)
import Noll.Label (Label (..))
import Noll.Utils (Name, forM)
import TextShow (showt)

import qualified Data.Text as Text
import qualified Noll.Core.Language as Core

runLifting :: RWS Name ObjectList Int a -> (a, ObjectList)
runLifting e = evalRWS e "" 1

liftLambdas :: ObjectList -> ObjectList
liftLambdas objs = objs1 <> objs2
 where
  (objs1, objs2) =
    runLifting (traverse (traverse go) objs)
  go =
    cata $
      \case
        Core.ELet vs e -> do
          ws <- forM vs $ \(Binding ll@(Label _ name) e1) -> do
            f <- local (const name) e1
            pure (Binding ll f)
          f <- local mempty e
          pure (Core.let_ ws f)
        Core.ELam vs e -> do
          n <- supplied id
          name <- ask
          f <- local mempty e
          moveUp (if Text.null name then "$fn." <> showt n else name) vs f
        e ->
          local mempty (embed <$> sequence e)

moveUp :: (MonadWriter ObjectList m) => Name -> List1 (Label Type) -> Expr Type -> m (Expr Type)
moveUp name vs f = do
  tell [OFunction name (fromList1 vs) f]
  pure (Core.var (Label (functionTypeOf f vs) name))
