{-# LANGUAGE LambdaCase #-}

module Coal.Compiler.Kernel.NewTranslate.Primitive (translatePrimitive) where

import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Language (Primitive (..))

translatePrimitive :: Primitive -> Prim
translatePrimitive =
  \case
    LUnit ->
      PUnit
    LBool b ->
      PBool b
    LInt32 n ->
      PInt32 n
    LInt64 n ->
      PInt64 n
    LBignum n ->
      PBignum n
    LFloat f ->
      PFloat f
    LDouble d ->
      PDouble d
    LChar c ->
      PChar c
    LString s ->
      PString s
