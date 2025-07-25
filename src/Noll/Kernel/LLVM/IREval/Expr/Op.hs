{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Kernel.LLVM.IREval.Expr.Op (irEvalOp) where

import Noll.Kernel.LLVM.IREval
import Noll.Kernel.LLVM.IREval.Comment (irCommentBlock)
import Noll.Kernel.LLVM.IRInstruction (FCmpCond (..), ICmpCond (..), IRInstr)
import Noll.Kernel.LLVM.IRInstruction.TH
import Noll.Kernel.LLVM.IRType (IRType (..))
import Noll.Kernel.LLVM.IRType.Syntax (i1, i32, i64)
import Noll.Kernel.LLVM.IRValue (IRValue (..))
import Noll.Kernel.Language (Op)

import qualified Noll.Kernel.LLVM.IRInstruction.TH as IR
import qualified Noll.Kernel.Language as Core

irEvalOp :: (IREval e) => Op e -> IRInstr IRValue
irEvalOp =
  \case
    Core.OAddInt32 e1 e2 -> do
      irCommentBlock "OAddInt32" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        add i32 v1 v2
    Core.OAddInt64 e1 e2 -> do
      irCommentBlock "OAddInt64" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        add i64 v1 v2
    Core.OAddFloat e1 e2 -> do
      irCommentBlock "OAddFloat" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        fadd TFloat v1 v2
    Core.OAddDouble e1 e2 -> do
      irCommentBlock "OAddDouble" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        fadd TDouble v1 v2
    Core.OSubInt32 e1 e2 -> do
      irCommentBlock "OSubInt32" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        sub i32 v1 v2
    Core.OSubInt64 e1 e2 -> do
      irCommentBlock "OSubInt64" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        sub i64 v1 v2
    Core.OSubFloat e1 e2 -> do
      irCommentBlock "OSubFloat" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        fsub TFloat v1 v2
    Core.OSubDouble e1 e2 -> do
      irCommentBlock "OSubDouble" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        fsub TDouble v1 v2
    Core.OMulInt32 e1 e2 -> do
      irCommentBlock "OMulInt32" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        mul i32 v1 v2
    Core.OMulInt64 e1 e2 -> do
      irCommentBlock "OMulInt64" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        mul i64 v1 v2
    Core.OMulFloat e1 e2 -> do
      irCommentBlock "OMulFloat" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        fmul TFloat v1 v2
    Core.OMulDouble e1 e2 -> do
      irCommentBlock "OMulDouble" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        fmul TDouble v1 v2
    Core.ODivInt32 e1 e2 -> do
      irCommentBlock "ODivInt32" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        udiv i32 v1 v2
    Core.ODivInt64 e1 e2 -> do
      irCommentBlock "ODivInt64" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        udiv i64 v1 v2
    Core.ODivFloat e1 e2 -> do
      irCommentBlock "ODivFloat" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        fdiv TFloat v1 v2
    Core.ODivDouble e1 e2 -> do
      irCommentBlock "ODivDouble" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        fdiv TDouble v1 v2
    Core.OEqInt32 e1 e2 -> do
      irCommentBlock "OEqInt32" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        icmp Eq i1 v1 v2
    Core.OEqInt64 e1 e2 -> do
      irCommentBlock "OEqInt64" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        icmp Eq i1 v1 v2
    Core.OEqFloat e1 e2 -> do
      irCommentBlock "OEqFloat" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        fcmp OEq i1 v1 v2
    Core.OEqDouble e1 e2 -> do
      irCommentBlock "OEqDouble" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        fcmp OEq i1 v1 v2
    Core.ONeInt32 e1 e2 -> do
      irCommentBlock "ONeInt32" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        icmp Ne i1 v1 v2
    Core.ONeInt64 e1 e2 -> do
      irCommentBlock "ONeInt64" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        icmp Ne i1 v1 v2
    Core.ONeFloat e1 e2 -> do
      irCommentBlock "ONeFloat" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        icmp Ne i1 v1 v2
    Core.ONeDouble e1 e2 -> do
      irCommentBlock "ONeDouble" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        icmp Ne i1 v1 v2
    Core.OAnd e1 e2 -> do
      irCommentBlock "OAnd" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        IR.and i1 v1 v2
    Core.OOr e1 e2 -> do
      irCommentBlock "OOr" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        IR.or i1 v1 v2
    Core.OLtInt32 e1 e2 -> do
      irCommentBlock "OLtInt32" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        icmp SLt i1 v1 v2
    Core.OLtInt64 e1 e2 -> do
      irCommentBlock "OLtInt64" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        icmp SLt i1 v1 v2
    Core.OLtFloat e1 e2 -> do
      irCommentBlock "OLtFloat" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        fcmp OLt i1 v1 v2
    Core.OLtDouble e1 e2 -> do
      irCommentBlock "OLtDouble" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        fcmp OLt i1 v1 v2
    Core.OLteInt32 e1 e2 -> do
      irCommentBlock "OLteInt32" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        icmp SLte i1 v1 v2
    Core.OLteInt64 e1 e2 -> do
      irCommentBlock "OLteInt64" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        icmp SLte i1 v1 v2
    Core.OLteFloat e1 e2 -> do
      irCommentBlock "OLteFloat" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        fcmp OLte i1 v1 v2
    Core.OLteDouble e1 e2 -> do
      irCommentBlock "OLteDouble" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        fcmp OLte i1 v1 v2
    Core.OGtInt32 e1 e2 -> do
      irCommentBlock "OGtInt32" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        icmp SGt i1 v1 v2
    Core.OGtInt64 e1 e2 -> do
      irCommentBlock "OGtInt64" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        icmp SGt i1 v1 v2
    Core.OGtFloat e1 e2 -> do
      irCommentBlock "OGtFloat" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        fcmp OGt i1 v1 v2
    Core.OGtDouble e1 e2 -> do
      irCommentBlock "OGtDouble" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        fcmp OGt i1 v1 v2
    Core.OGteInt32 e1 e2 -> do
      irCommentBlock "OGteInt32" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        icmp SGte i1 v1 v2
    Core.OGteInt64 e1 e2 -> do
      irCommentBlock "OGteInt64" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        icmp SGte i1 v1 v2
    Core.OGteFloat e1 e2 -> do
      irCommentBlock "OGteFloat" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        fcmp OGte i1 v1 v2
    Core.OGteDouble e1 e2 -> do
      irCommentBlock "OGteDouble" $ do
        v1 <- irEval e1
        v2 <- irEval e2
        fcmp OGte i1 v1 v2
    Core.ONot e -> do
      irCommentBlock "ONot" $ do
        v1 <- irEval e
        xor i1 v1 (I1 True)
