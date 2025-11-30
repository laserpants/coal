{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Kernel.TranslateType (translateType) where

import qualified Coal.Kernel.Language as Kernel
import Coal.Language.Type (Type (..), isTupleType)
import Coal.Language.Type.Intrinsic (Intrinsic (..))
import Coal.Language.Type.Row (Row (..))

{-# INLINE tupleTCon #-}
tupleTCon :: Kernel.Type
tupleTCon = Kernel.TCon "tuple" []

{-# INLINE listTCon #-}
listTCon :: Kernel.Type
listTCon = Kernel.TCon "list" []

{-# INLINE natTCon #-}
natTCon :: Kernel.Type
natTCon = Kernel.TCon "nat" []

{-# INLINE voidTCon #-}
voidTCon :: Kernel.Type
voidTCon = Kernel.TCon "void" []

translateIntrinsicType :: Intrinsic -> Kernel.Type
translateIntrinsicType =
  \case
    IBool ->
      Kernel.bool
    IChar ->
      Kernel.char
    IDouble ->
      Kernel.double
    IFloat ->
      Kernel.float
    IInt32 ->
      Kernel.int32
    IInt64 ->
      Kernel.int64
    IBignum ->
      Kernel.bignum
    IString ->
      Kernel.string
    IUnit ->
      Kernel.unit
    INat ->
      natTCon
    IVoid ->
      voidTCon

translateRow :: Row o k (Type o k) -> Kernel.Type
translateRow =
  \case
    RExtend name t r ->
      Kernel.RExt name (translateType t) (translateRow r)
    RVariable{} ->
      Kernel.TOpq
    RNil ->
      Kernel.RNil

translateApplication :: Kernel.Type -> Kernel.Type -> Kernel.Type
translateApplication t (Kernel.TCon name ts) = Kernel.TCon name (ts <> [t])
translateApplication _ _ = Kernel.TOpq

translateType :: Type o k -> Kernel.Type
translateType =
  \case
    TApplication _ t1 t2 ->
      translateApplication (translateType t1) (translateType t2)
    TArrow t1 t2 ->
      Kernel.arrow (translateType t1) (translateType t2)
    con@TConstructor{}
      | isTupleType con ->
          tupleTCon
    TConstructor _ "List" ->
      listTCon
    TConstructor _ name ->
      Kernel.TCon name []
    TIntrinsic t ->
      translateIntrinsicType t
    TRecord t ->
      Kernel.record (translateType t)
    TRow r ->
      translateRow r
    TVariable{} ->
      Kernel.TOpq
    TAlias _ _ t ->
      translateType t
