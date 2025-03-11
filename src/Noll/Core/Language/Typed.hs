{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.Language.Typed (Typed (..)) where

import Noll.Core.Language.Op (Op (..))
import Noll.Core.Language.Prim (Prim (..))
import Noll.Core.Language.Type (Type (..))
import Noll.Label (Label (..))

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
      OLtEInt32{} ->
        Type.bool
      OLtEInt64{} ->
        Type.bool
      OGtInt32{} ->
        Type.bool
      OGtInt64{} ->
        Type.bool
      OGtEInt32{} ->
        Type.bool
      OGtEInt64{} ->
        Type.bool
      OEqInt32{} ->
        Type.bool
      OEqInt64{} ->
        Type.bool
      ONeInt32{} ->
        Type.bool
      ONeInt64{} ->
        Type.bool
      OAnd{} ->
        Type.bool
      OOr{} ->
        Type.bool
      ONot{} ->
        Type.bool
      OAddInt32{} ->
        Type.int32
      OAddInt64{} ->
        Type.int64
      OSubInt32{} ->
        Type.int32
      OSubInt64{} ->
        Type.int64
      OMulInt32{} ->
        Type.int32
      OMulInt64{} ->
        Type.int64
      OMulFloat{} ->
        Type.float
      OMulDouble{} ->
        Type.double
      ODivInt32{} ->
        Type.int32
      ODivInt64{} ->
        Type.int64
      ODivFloat{} ->
        Type.float
      ODivDouble{} ->
        Type.double
