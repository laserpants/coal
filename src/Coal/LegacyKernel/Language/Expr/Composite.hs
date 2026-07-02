{-# LANGUAGE OverloadedStrings #-}

module Coal.LegacyKernel.Language.Expr.Composite (tupleExpr, cons, recordExpr) where

import Coal.Common.Label (Label (..))
import Coal.LegacyKernel.Language.Expr (Expr)
import Coal.LegacyKernel.Language.Expr.Syntax (app, var)
import Coal.LegacyKernel.Language.Type.Arrow (foldType)
import Coal.LegacyKernel.Language.Type.Syntax (arrow)
import qualified Coal.LegacyKernel.Language.Type.Syntax as Lang
import Coal.LegacyKernel.Language.Typed (Typed (..))
import Data.List.NonEmpty (NonEmpty (..), toList)
import TextShow (showt)

tupleExpr :: NonEmpty (Expr Lang.Type) -> Expr Lang.Type
tupleExpr es = app t (var (Label (foldType t ts) ("$Tuple" <> showt n))) es
 where
  n = length es
  t = Lang.tuple (toList ts)
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
