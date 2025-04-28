{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Noll.Compiler.Lowpass.TranslateType where

import Noll.Language.Type
import Noll.Language.Type.Row
import Noll.Language.Type.Intrinsic

import qualified Lang.Lowpass.Language as Lowpass

translateIntrinsicType :: Intrinsic (Type o k) -> Lowpass.Type
translateIntrinsicType =
  \case
    IBool ->
      Lowpass.bool
    IChar ->
      Lowpass.char
    IDouble ->
      Lowpass.double
    IFloat ->
      Lowpass.float
    IInt32 ->
      Lowpass.int32
    IInt64 ->
      Lowpass.int64
    IBignum ->
      Lowpass.bignum
    IList t ->
      Lowpass.list (translateType t)
    IString ->
      Lowpass.string
    IUnit ->
      Lowpass.unit
    ITuple ts ->
      Lowpass.tuple (translateType <$> ts)
    IRecord t ->
      Lowpass.record (translateType t)
    INat ->
      Lowpass.TCon "nat" []
    IOption t ->
      Lowpass.TCon "option" [translateType t]
    IResult t ->
      Lowpass.TCon "result" [translateType t]
    IVoid ->
      Lowpass.TCon "void" []

translateRow :: Row o k (Type o k) -> Lowpass.Type
translateRow =
  \case
    RExtend name t r ->
      Lowpass.RExt name (translateType t) (translateRow r)
    RVariable{} ->
      Lowpass.TOpq
    RNil ->
      Lowpass.RNil

translateApplication :: Lowpass.Type -> Lowpass.Type -> Lowpass.Type
translateApplication t (Lowpass.TCon name ts) = Lowpass.TCon name (ts <> [t])
translateApplication _ _ = error "Implementation error"

translateType :: Type o k -> Lowpass.Type
translateType =
  \case
    TApplication _ t ts ->
      foldr translateApplication (translateType t) (translateType <$> ts)
    TArrow t1 t2 ->
      Lowpass.arrow (translateType t1) (translateType t2)
    TConstructor _ name ->
      Lowpass.TCon name []
    TIntrinsic t ->
      translateIntrinsicType t
    TRow r ->
      translateRow r
    TVariable{} ->
      Lowpass.TOpq
    TAlias _ _ t ->
      translateType t
