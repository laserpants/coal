{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Compiler.Pass.ExtraArgs (addImplicitArgs) where

import Coal.Common.Label (Label (..))
import Coal.Common.List1 (List1, NonEmpty (..))
import Coal.Kernel.Compiler.Ast (flattenAppNodes)
import Coal.Kernel.Language (Expr, Type, Typed (..))
import Coal.Kernel.Language.Object (Object (..))
import TextShow (showt)

import qualified Coal.Common.List1 as List1
import qualified Coal.Kernel.Language as Core

addImplicitArgs :: Object Type (Expr Type) -> Object Type (Expr Type)
addImplicitArgs =
  \case
    f@(OFunction name lls1 expr)
      | isExprFun ->
          OFunction
            name
            (lls1 <> lls2)
            (flattenAppNodes (Core.app (List1.last ts) expr (exprs lls2)))
      | otherwise ->
          f
     where
      isExprFun =
        length ts > 1
      ts =
        Core.unfoldType (typeOf expr)
      lls2 =
        labels (List1.init ts)
    o ->
      o

exprs :: [Label t] -> List1 (Expr t)
exprs (ll : lls) = Core.var <$> ll :| lls
exprs _ = error "Implementation error"

labels :: [a] -> [Label a]
labels ts = zipWith Label ts ["$extra." <> showt i | i <- [0 :: Int ..]]
