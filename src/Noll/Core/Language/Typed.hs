{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.Language.Typed (Typed (..), isFunction) where

import Data.Functor.Foldable (project)
import Noll.Core.Language.Expr (Expr, ExprF (..))
import Noll.Core.Language.Op (Op (..))
import Noll.Core.Language.Prim (Prim (..))
import Noll.Core.Language.Syntax.Type (arity)
import Noll.Core.Language.Type (Type)
import Noll.Label (Label (..))

import qualified Noll.Core.Language.Syntax.Type as Type

class Typed t where
  typeOf :: t -> Type

instance Typed Type where
  typeOf = id

instance Typed Prim where
  typeOf =
    \case
      PUnit{} ->
        Type.unit
      PBool{} ->
        Type.bool
      PInt32{} ->
        Type.int32
      PInt64{} ->
        Type.int64
      PFloat{} ->
        Type.float
      PDouble{} ->
        Type.double
      PChar{} ->
        Type.char
      PString{} ->
        Type.string

instance (Typed t) => Typed (Label t) where
  typeOf (Label t _) = typeOf t

instance Typed (Op a) where
  typeOf =
    \case
      OLtInt32{} ->
        Type.bool
      OLtInt64{} ->
        Type.bool
      OGtInt32{} ->
        Type.bool
      OGtInt64{} ->
        Type.bool
      OEqInt32{} ->
        Type.bool
      OEqInt64{} ->
        Type.bool
      OAnd{} ->
        Type.bool
      OOr{} ->
        Type.bool
      _ ->
        error "TODO"

instance (Typed t, Typed a) => Typed (ExprF t a) where
  typeOf =
    \case
      EVar t ->
        typeOf t
      ELit t ->
        typeOf t
      ELet _ t ->
        typeOf t
      EIf _ _ t ->
        typeOf t
      EApp t _ _ ->
        typeOf t

--      EPat _ (t :| _) ->
--        typeOf t
--      ESel _ _ t ->
--        typeOf t
--      EOp op ->
--        typeOf op
--      ECall _ _ t ->
--        returnTypeOf t
--      ENil ->
--        Type.RNil
--      EMem t ->
--        typeOf t
--      ELam ts t ->
--        foldType (typeOf t) (typeOf <$> ts)
--      EExt n t1 t2 ->
--        error "TODO" -- normalizeRow (RExt n (typeOf t1) (typeOf t2))

instance (Typed t) => Typed (Expr t) where
  typeOf = typeOf . project

{-# INLINE isFunction #-}
isFunction :: (Typed t) => t -> Bool
isFunction f = arity (typeOf f) > 0
