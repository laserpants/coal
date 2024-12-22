{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.HasType (HasType (..)) where

import Noll.Label (Label (..))
import Noll.Language.Expression (Expression)
import qualified Noll.Language.Expression as Expr
import Noll.Language.Pattern (Pattern (..))
import qualified Noll.Language.Pattern as Pattern
import Noll.Language.Primitive (Primitive)
import qualified Noll.Language.Primitive as Prim
import Noll.Language.Type (Type (..), foldType)
import Noll.Language.Type.Index (TypeIndex)
import Noll.Language.Type.Intrinsic (Intrinsic (..))

class HasType o k t where
  typeOf :: t -> Type o k

instance HasType o k (Type o k) where
  typeOf = id

instance (HasType o k t) => HasType o k (Label t) where
  typeOf =
    \case
      Label t _ ->
        typeOf t

instance HasType o k Primitive where
  typeOf =
    \case
      Prim.Unit ->
        Intrinsic Unit
      Prim.Bool{} ->
        Intrinsic Bool
      Prim.Int32{} ->
        Intrinsic Int32
      Prim.Int64{} ->
        Intrinsic Int64
      Prim.Float{} ->
        Intrinsic Float
      Prim.Double{} ->
        Intrinsic Double
      Prim.Char{} ->
        Intrinsic Char
      Prim.String{} ->
        Intrinsic String

instance HasType o k (Pattern (Type o k)) where
  typeOf =
    \case
      Pattern.Variable t ->
        typeOf t
      Pattern.Constructor t _ ->
        typeOf t

instance HasType o k (Expression (Type o k)) where
  typeOf =
    \case
      Expr.Literal t ->
        typeOf t
      Expr.Constructor t ->
        typeOf t
      Expr.Variable t ->
        typeOf t
      Expr.Application t _ _ ->
        typeOf t
      Expr.If _ _ t ->
        typeOf t
      Expr.Let _ t ->
        typeOf t
      Expr.Lambda ts t ->
        foldType (typeOf t) (typeOf <$> ts)
      Expr.BinaryOperator (t, _) ->
        typeOf t
      Expr.UnaryOperator (t, _) ->
        typeOf t
      Expr.Record t _ _ ->
        typeOf t
      Expr.ListCons t _ _ ->
        typeOf t
      Expr.ListLiteral t _ ->
        typeOf t

--      Expr.Match t _ _ _ ->
--        typeOf t
--      Expr.Fold t _ _ _ ->
--        typeOf t
