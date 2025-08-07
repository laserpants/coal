{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Kernel.TranslateType (translateType) where

import Coal.Language.Type
import Coal.Language.Type.Intrinsic
import Coal.Language.Type.Row
import Data.Text (isPrefixOf)

import qualified Coal.Kernel.Language as Kernel

translateIntrinsicType :: Intrinsic (Type o k) -> Kernel.Type
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
    IRecord t ->
      Kernel.record (translateType t)
    INat ->
      Kernel.TCon "nat" []
    IVoid ->
      Kernel.TCon "void" []

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
    TApplication _ t ts ->
      foldr (translateApplication . translateType) (translateType t) ts
    TArrow t1 t2 ->
      Kernel.arrow (translateType t1) (translateType t2)
    TConstructor _ con
      | "#Tuple" `isPrefixOf` con ->
          Kernel.TCon "tuple" []
    TConstructor _ "List" ->
      Kernel.TCon "list" []
    TConstructor _ name ->
      Kernel.TCon name []
    TIntrinsic t ->
      translateIntrinsicType t
    TRow r ->
      translateRow r
    TVariable{} ->
      Kernel.TOpq
    TAlias _ _ t ->
      translateType t
