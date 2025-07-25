{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Kernel.Compiler.Pass.ExtraArgs (addImplicitArgs) where

import Noll.Common.Label (Label (..))
import Noll.Common.List1 (List1, NonEmpty (..))
import Noll.Kernel.Compiler.Ast (flattenAppNodes)
import Noll.Kernel.Language (Expr, Type, Typed (..))
import Noll.Kernel.Language.Object (Object (..))
import TextShow (showt)

import qualified Noll.Common.List1 as List1
import qualified Noll.Kernel.Language as Core

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
