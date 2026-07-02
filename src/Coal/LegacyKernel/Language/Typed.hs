{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.LegacyKernel.Language.Typed (Typed (..)) where

import Coal.Common.Label (Label (..))
import Coal.LegacyKernel.Language.Op (Op (..))
import Coal.LegacyKernel.Language.Prim (Prim (..))
import Coal.LegacyKernel.Language.Type (Type (..))
import qualified Coal.LegacyKernel.Language.Type.Syntax as Type

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
      PBignum{} ->
        Type.bignum
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
      OLtFloat{} ->
        Type.bool
      OLtDouble{} ->
        Type.bool
      OLteInt32{} ->
        Type.bool
      OLteInt64{} ->
        Type.bool
      OLteFloat{} ->
        Type.bool
      OLteDouble{} ->
        Type.bool
      OGtInt32{} ->
        Type.bool
      OGtInt64{} ->
        Type.bool
      OGtFloat{} ->
        Type.bool
      OGtDouble{} ->
        Type.bool
      OGteInt32{} ->
        Type.bool
      OGteInt64{} ->
        Type.bool
      OGteFloat{} ->
        Type.bool
      OGteDouble{} ->
        Type.bool
      OEqInt32{} ->
        Type.bool
      OEqInt64{} ->
        Type.bool
      OEqFloat{} ->
        Type.bool
      OEqDouble{} ->
        Type.bool
      ONeInt32{} ->
        Type.bool
      ONeInt64{} ->
        Type.bool
      ONeFloat{} ->
        Type.bool
      ONeDouble{} ->
        Type.bool
      OAnd{} ->
        Type.bool
      OOr{} ->
        Type.bool
      ONot{} ->
        Type.bool
      ONegFloat{} ->
        Type.float
      ONegDouble{} ->
        Type.double
      OAddInt32{} ->
        Type.int32
      OAddInt64{} ->
        Type.int64
      OAddFloat{} ->
        Type.float
      OAddDouble{} ->
        Type.double
      OSubInt32{} ->
        Type.int32
      OSubInt64{} ->
        Type.int64
      OSubFloat{} ->
        Type.float
      OSubDouble{} ->
        Type.double
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
      OEqChar{} ->
        Type.bool
      OEqBool{} ->
        Type.bool
      ONeChar{} ->
        Type.bool
      ONeBool{} ->
        Type.bool
