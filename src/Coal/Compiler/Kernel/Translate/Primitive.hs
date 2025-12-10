{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Coal.Compiler.Kernel.Translate.Primitive (translatePrimitive) where

import qualified Coal.Kernel.Language as Kernel
import Coal.Language

translatePrimitive :: Primitive -> Kernel.Prim
translatePrimitive =
  \case
    LUnit ->
      Kernel.PUnit
    LBool bool ->
      Kernel.PBool bool
    LInt32 int32 ->
      Kernel.PInt32 int32
    LInt64 int64 ->
      Kernel.PInt64 int64
    LBignum int ->
      Kernel.PBignum int
    LFloat float ->
      Kernel.PFloat float
    LDouble double ->
      Kernel.PDouble double
    LChar chr ->
      Kernel.PChar chr
    LString str ->
      Kernel.PString str
