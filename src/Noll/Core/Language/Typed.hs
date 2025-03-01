{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.Language.Typed (Typed (..), isFunction) where

import Data.Functor.Foldable (project)
import Noll.Core.Language.Expr (Expr, ExprF (..))
import Noll.Core.Language.Op (Op (..))
import Noll.Core.Language.Prim (Prim (..))
import Noll.Core.Language.Type (Type (..), normalizeRow)
import Noll.Core.Language.Type.Syntax (arity, foldType, unfoldType)
import Noll.Label (Label (..))

import qualified Noll.Common.List1 as List1
import qualified Noll.Core.Language.Type.Syntax as Type

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
      OAddInt32{} ->
        Type.int32
      OAddInt64{} ->
        Type.int64
      OSubInt32{} ->
        Type.int32
      OSubInt64{} ->
        Type.int64
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
      EMat t _ _ ->
        typeOf t
      ESel _ _ t ->
        typeOf t
      EOp op ->
        typeOf op
      ENil ->
        Type.RNil
      EExt (Label _ n) t1 t2 ->
        normalizeRow (RExt n (typeOf t1) (typeOf t2))
      ELam ts t ->
        foldType (typeOf t) (typeOf <$> ts)
      ECall _ _ t ->
        returnTypeOf t

instance (Typed t) => Typed (Expr t) where
  typeOf = typeOf . project

{-# INLINE isFunction #-}
isFunction :: (Typed t) => t -> Bool
isFunction f = arity (typeOf f) > 0

{-# INLINE returnTypeOf #-}
returnTypeOf :: (Typed t) => t -> Type
returnTypeOf = List1.last . unfoldType . typeOf
