{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Language.Expr.Composite (tupleExpr, cons, recordExpr) where

import Coal.Common.Label (Label (..))
import Coal.Common.List1 (List1, NonEmpty (..), fromList1)
import Coal.Kernel.Language.Expr (Expr)
import Coal.Kernel.Language.Expr.Syntax
import Coal.Kernel.Language.Type.Arrow (foldType)
import Coal.Kernel.Language.Type.Syntax (arrow)
import Coal.Kernel.Language.Typed (Typed (..))
import TextShow (showt)

import qualified Coal.Kernel.Language.Type.Syntax as Lang

tupleExpr :: List1 (Expr Lang.Type) -> Expr Lang.Type
tupleExpr es = app t (var (Label (foldType t ts) ("$Tuple" <> showt n))) es
 where
  n = length es
  t = Lang.tuple (fromList1 ts)
  ts = typeOf <$> es

cons :: Expr Lang.Type -> Expr Lang.Type -> Expr Lang.Type
cons x xs =
  app
    (Lang.list t)
    (var (Label (t `arrow` Lang.list t `arrow` Lang.list t) "$Cons"))
    (x :| [xs])
 where
  t = typeOf x

recordExpr :: Expr Lang.Type -> Expr Lang.Type
recordExpr r =
  app
    (Lang.record (typeOf r))
    (var (Label (t `arrow` Lang.record t) "$Record"))
    (r :| [])
 where
  t = typeOf r
