{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Primitive operations and literal compilation.

Handles code generation for:

  * Arithmetic operators (@+@, @-@, @*@, @/@)
  * Comparison operators (@==@, @!=@, @<@, @>@)
  * Logical operators (@!@)
  * String and bignum literals

Primitive literals are either compiled to IR constants or emitted as global
variables with initializers (for strings).
-}
module Coal.Kernel.LLVM.Prim (
  irOp,
  irPrim,
  irLiteralString,
  primToIRConstant,
) where

import Control.Monad.State (get, modify)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import LLVM.IR
import qualified LLVM.IROperand.Constructors as O

import Coal.Kernel.LLVM.Monad (IRCodegen)
import Coal.Kernel.LLVM.Runtime (callRuntime)
import Coal.Kernel.LLVM.RuntimeDefs (rtBignumNew, rtStringNew)
import Coal.Kernel.Language.Expr (Expr)
import Coal.Kernel.Language.Op (Op (..))
import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Kernel.Language.Type (Type)

irOp :: (Expr Type -> IRCodegen IROperand) -> Op (Expr Type) -> IRCodegen IROperand
irOp irValue =
  \case
    OAddInt32 e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      add i32 o1 o2
    OAddInt64 e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      add i64 o1 o2
    OAddFloat e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      fadd TFloat o1 o2
    OAddDouble e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      fadd TDouble o1 o2
    OSubInt32 e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      sub i32 o1 o2
    OSubInt64 e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      sub i64 o1 o2
    OSubFloat e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      fsub TFloat o1 o2
    OSubDouble e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      fsub TDouble o1 o2
    OMulInt32 e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      mul i32 o1 o2
    OMulInt64 e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      mul i64 o1 o2
    OMulFloat e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      fmul TFloat o1 o2
    OMulDouble e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      fmul TDouble o1 o2
    ODivInt32 e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      udiv i32 o1 o2
    ODivInt64 e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      udiv i64 o1 o2
    ODivFloat e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      fdiv TFloat o1 o2
    ODivDouble e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      fdiv TDouble o1 o2
    OEqInt32 e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      icmp ICmpEq i32 o1 o2
    OEqInt64 e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      icmp ICmpEq i64 o1 o2
    OEqFloat e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      fcmp FCmpOEq TFloat o1 o2
    OEqDouble e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      fcmp FCmpOEq TDouble o1 o2
    OEqChar e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      icmp ICmpEq i32 o1 o2
    OEqBool e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      icmp ICmpEq i1 o1 o2
    ONeInt32 e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      icmp ICmpNe i32 o1 o2
    ONeInt64 e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      icmp ICmpNe i64 o1 o2
    ONeFloat e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      fcmp FCmpONe TFloat o1 o2
    ONeDouble e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      fcmp FCmpONe TDouble o1 o2
    ONeChar e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      icmp ICmpNe i32 o1 o2
    ONeBool e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      icmp ICmpNe i1 o1 o2
    OLtInt32 e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      icmp ICmpSLt i32 o1 o2
    OLtInt64 e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      icmp ICmpSLt i64 o1 o2
    OLtFloat e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      fcmp FCmpOLt TFloat o1 o2
    OLtDouble e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      fcmp FCmpOLt TDouble o1 o2
    OLteInt32 e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      icmp ICmpSLe i32 o1 o2
    OLteInt64 e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      icmp ICmpSLe i64 o1 o2
    OLteFloat e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      fcmp FCmpOLe TFloat o1 o2
    OLteDouble e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      fcmp FCmpOLe TDouble o1 o2
    OGtInt32 e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      icmp ICmpSGt i32 o1 o2
    OGtInt64 e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      icmp ICmpSGt i64 o1 o2
    OGtFloat e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      fcmp FCmpOGt TFloat o1 o2
    OGtDouble e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      fcmp FCmpOGt TDouble o1 o2
    OGteInt32 e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      icmp ICmpSGe i32 o1 o2
    OGteInt64 e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      icmp ICmpSGe i64 o1 o2
    OGteFloat e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      fcmp FCmpOGe TFloat o1 o2
    OGteDouble e1 e2 -> do
      o1 <- irValue e1
      o2 <- irValue e2
      fcmp FCmpOGe TDouble o1 o2
    ONot e -> do
      o1 <- irValue e
      xor i1 o1 O.true
    ONegInt32 e -> do
      o1 <- irValue e
      sub i32 (O.i32 @Int 0) o1
    ONegInt64 e -> do
      o1 <- irValue e
      sub i64 (O.i64 @Int 0) o1
    ONegFloat e -> do
      o1 <- irValue e
      fneg TFloat o1
    ONegDouble e -> do
      o1 <- irValue e
      fneg TDouble o1
    OAnd{} ->
      error "Internal error: Unexpected OAnd"
    OOr{} ->
      error "Internal error: Unexpected OOr"

irPrim :: Prim -> IRCodegen IROperand
irPrim =
  \case
    PBool True ->
      return O.true
    PBool False ->
      return O.false
    PInt32 n ->
      return (O.i32 n)
    PInt64 n ->
      return (O.i64 n)
    PFloat f ->
      return (O.float f)
    PDouble d ->
      return (O.double d)
    PUnit ->
      return (O.nullPtr TPtr)
    PChar c ->
      return (O.i32 c)
    PString bs -> do
      ptr1 <- irLiteralString bs
      callRuntime rtStringNew [ptr1]
    PBignum n -> do
      let bs = Text.encodeUtf8 (Text.pack (show n))
      ptr1 <- irLiteralString bs
      callRuntime rtBignumNew [ptr1]

{- | Emit a private global bytestring constant (null-terminated) and return a
pointer to it.  Identical byte strings within the same compilation unit are
deduped: the second and subsequent calls return the operand for the first
global that was emitted.  Used to compile 'PString' and 'PBignum' literals.
-}
irLiteralString :: ByteString -> IRCodegen IROperand
irLiteralString bs = do
  (_, n, cache) <- get
  case Map.lookup bs cache of
    Just litName ->
      return (OGlobal TPtr litName)
    Nothing -> do
      let litName = ".str_lit_" <> Text.pack (show n)
      modify (\(s, i, m) -> (s, i + 1, Map.insert bs litName m))
      emitGlobal (IRString LPrivate litName (bs <> ByteString.singleton 0))
      return (OGlobal TPtr litName)

-- | Convert a primitive literal to an IR constant, if representable directly.
primToIRConstant :: Prim -> Maybe (IRType, IRConstant)
primToIRConstant =
  \case
    PBool True ->
      Just (i1, CInt 1 1)
    PBool False ->
      Just (i1, CInt 1 0)
    PInt32 n ->
      Just (i32, CInt 32 (fromIntegral n))
    PInt64 n ->
      Just (i64, CInt 64 (fromIntegral n))
    PFloat f ->
      Just (TFloat, CFloat f)
    PDouble d ->
      Just (TDouble, CFloat (realToFrac d))
    PUnit ->
      Just (i1, CInt 1 0)
    _ ->
      Nothing
