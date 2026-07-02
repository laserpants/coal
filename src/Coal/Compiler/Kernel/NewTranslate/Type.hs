{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Kernel.NewTranslate.Type (translateType) where

import qualified Coal.Kernel.Language.Type as NK
import qualified Coal.Kernel.Language.Type.Constructors as NKT
import Coal.Language.Type (Type (..))
import Coal.Language.Type.Intrinsic (Intrinsic (..))
import Coal.Language.Type.Operations (typeArgs)
import Coal.Language.Type.Row (Row (..))
import qualified Data.Text as Text

{-# INLINE tupleTCon #-}
tupleTCon :: NK.Type
tupleTCon = NK.TCon "tuple" []

{-# INLINE listTCon #-}
listTCon :: NK.Type
listTCon = NK.TCon "list" []

{-# INLINE natTCon #-}
natTCon :: NK.Type
natTCon = NK.TCon "nat" []

{-# INLINE voidTCon #-}
voidTCon :: NK.Type
voidTCon = NK.TCon "void" []

translateIntrinsicType :: Intrinsic -> NK.Type
translateIntrinsicType =
  \case
    IBool ->
      NKT.bool
    IChar ->
      NKT.char
    IDouble ->
      NKT.double
    IFloat ->
      NKT.float
    IInt32 ->
      NKT.int32
    IInt64 ->
      NKT.int64
    IBignum ->
      NKT.bignum
    IString ->
      NKT.string
    IUnit ->
      NKT.unit
    INat ->
      natTCon
    IVoid ->
      voidTCon

translateRow :: Row o k (Type o k) -> NK.Type
translateRow =
  \case
    RExtend name t r ->
      NK.RExt name (translateType t) (translateRow r)
    RVariable{} ->
      NK.TOpq
    RNil ->
      NK.RNil

translateApplication :: NK.Type -> NK.Type -> NK.Type
translateApplication t (NK.TCon name ts) = NK.TCon name (t : ts)
translateApplication _ _ = NK.TOpq

translateType :: Type o k -> NK.Type
translateType =
  \case
    t@TApplication{} ->
      let (t1, ts) = typeArgs t
       in foldr (translateApplication . translateType) (translateType t1) ts
    TArrow t1 t2 ->
      NKT.arrow (translateType t1) (translateType t2)
    TConstructor _ con
      | "#Tuple" `Text.isPrefixOf` con ->
          tupleTCon
    TConstructor _ "List" ->
      listTCon
    TConstructor _ name ->
      NK.TCon name []
    TIntrinsic t ->
      translateIntrinsicType t
    TRecord t ->
      NK.TCon "record" [translateType t]
    TRow r ->
      translateRow r
    TVariable{} ->
      NK.TOpq
    TAlias _ _ t ->
      translateType t
