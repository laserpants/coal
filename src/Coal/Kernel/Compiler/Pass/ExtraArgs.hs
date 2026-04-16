{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Compiler.Pass.ExtraArgs (addImplicitArgs) where

import Coal.Common.Label (Label (..))
import Coal.Kernel.Compiler.AST (flattenAppNodes)
import Coal.Kernel.Language (Expr, Type, Typed (..))
import qualified Coal.Kernel.Language as Syntax
import Coal.Kernel.Language.Object (Object (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import TextShow (showt)

addImplicitArgs :: Object Type (Expr Type) -> Object Type (Expr Type)
addImplicitArgs =
  \case
    f@(OFunction name lls1 expr)
      | isExprFun ->
          OFunction
            name
            (lls1 <> lls2)
            (flattenAppNodes (Syntax.app (NonEmpty.last ts) expr (exprs lls2)))
      | otherwise ->
          f
     where
      isExprFun =
        length ts > 1
      ts =
        Syntax.unfoldType (typeOf expr)
      lls2 =
        labels (NonEmpty.init ts)
    o ->
      o

exprs :: [Label t] -> NonEmpty (Expr t)
exprs (ll : lls) = Syntax.var <$> ll :| lls
exprs _ = error "Implementation error"

labels :: [a] -> [Label a]
labels ts = zipWith Label ts ["$extra." <> showt i | i <- [0 :: Int ..]]
