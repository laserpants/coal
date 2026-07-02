{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.LegacyKernel.LLVM.IREval.Expr.Op (irEvalOp) where

import Coal.LegacyKernel.LLVM.IREval (
  IREval (..),
  IRTailContext (NotInTail),
 )
import Coal.LegacyKernel.LLVM.IREval.Comment (irCommentBlock)
import Coal.LegacyKernel.LLVM.IRInstruction (FCmpCond (..), ICmpCond (..), IRInstr)
import Coal.LegacyKernel.LLVM.IRInstruction.Builders
import Coal.LegacyKernel.LLVM.IRType (IRType (..))
import Coal.LegacyKernel.LLVM.IRType.Syntax (i1, i32, i64)
import Coal.LegacyKernel.LLVM.IRValue (IRValue (..))
import Coal.LegacyKernel.Language (Op (..))

irEvalOp :: (IREval e) => Op e -> IRInstr IRValue
irEvalOp =
  \case
    OAddInt32 e1 e2 -> do
      irCommentBlock "OAddInt32" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        add i32 v1 v2
    OAddInt64 e1 e2 -> do
      irCommentBlock "OAddInt64" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        add i64 v1 v2
    OAddFloat e1 e2 -> do
      irCommentBlock "OAddFloat" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        fadd TFloat v1 v2
    OAddDouble e1 e2 -> do
      irCommentBlock "OAddDouble" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        fadd TDouble v1 v2
    OSubInt32 e1 e2 -> do
      irCommentBlock "OSubInt32" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        sub i32 v1 v2
    OSubInt64 e1 e2 -> do
      irCommentBlock "OSubInt64" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        sub i64 v1 v2
    OSubFloat e1 e2 -> do
      irCommentBlock "OSubFloat" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        fsub TFloat v1 v2
    OSubDouble e1 e2 -> do
      irCommentBlock "OSubDouble" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        fsub TDouble v1 v2
    OMulInt32 e1 e2 -> do
      irCommentBlock "OMulInt32" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        mul i32 v1 v2
    OMulInt64 e1 e2 -> do
      irCommentBlock "OMulInt64" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        mul i64 v1 v2
    OMulFloat e1 e2 -> do
      irCommentBlock "OMulFloat" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        fmul TFloat v1 v2
    OMulDouble e1 e2 -> do
      irCommentBlock "OMulDouble" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        fmul TDouble v1 v2
    ODivInt32 e1 e2 -> do
      irCommentBlock "ODivInt32" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        udiv i32 v1 v2
    ODivInt64 e1 e2 -> do
      irCommentBlock "ODivInt64" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        udiv i64 v1 v2
    ODivFloat e1 e2 -> do
      irCommentBlock "ODivFloat" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        fdiv TFloat v1 v2
    ODivDouble e1 e2 -> do
      irCommentBlock "ODivDouble" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        fdiv TDouble v1 v2
    OEqInt32 e1 e2 -> do
      irCommentBlock "OEqInt32" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        icmp Eq i1 v1 v2
    OEqInt64 e1 e2 -> do
      irCommentBlock "OEqInt64" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        icmp Eq i1 v1 v2
    OEqFloat e1 e2 -> do
      irCommentBlock "OEqFloat" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        fcmp OEq i1 v1 v2
    OEqDouble e1 e2 -> do
      irCommentBlock "OEqDouble" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        fcmp OEq i1 v1 v2
    OEqChar e1 e2 -> do
      irCommentBlock "OEqChar" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        icmp Eq i1 v1 v2
    OEqBool e1 e2 -> do
      irCommentBlock "OEqBool" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        icmp Eq i1 v1 v2
    ONeInt32 e1 e2 -> do
      irCommentBlock "ONeInt32" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        icmp Ne i1 v1 v2
    ONeInt64 e1 e2 -> do
      irCommentBlock "ONeInt64" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        icmp Ne i1 v1 v2
    ONeFloat e1 e2 -> do
      irCommentBlock "ONeFloat" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        icmp Ne i1 v1 v2
    ONeDouble e1 e2 -> do
      irCommentBlock "ONeDouble" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        icmp Ne i1 v1 v2
    ONeChar e1 e2 -> do
      irCommentBlock "ONeChar" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        icmp Ne i1 v1 v2
    ONeBool e1 e2 -> do
      irCommentBlock "ONeBool" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        icmp Ne i1 v1 v2
    OAnd e1 e2 -> do
      irCommentBlock "OAnd (short-circuit)" $ do
        -- Short-circuit AND: if e1 is false, return false without evaluating e2
        v1 <- irEval NotInTail e1
        labelThen <- label "and.then"
        labelFalse <- label "and.false"
        labelEnd <- label "and.end"
        br v1 [labelThen, labelFalse]

        -- False branch: short-circuit with false
        (labelFalseActual, _) <- block labelFalse $ do
          br1 labelEnd
          pure (I1 False)

        -- True branch: evaluate e2
        (labelThenActual, v2) <- block labelThen $ do
          v2 <- irEval NotInTail e2
          br1 labelEnd
          pure v2

        -- Merge results
        (_, result) <-
          block labelEnd $
            phi i1 [(labelFalseActual, I1 False), (labelThenActual, v2)]
        pure result
    OOr e1 e2 -> do
      irCommentBlock "OOr (short-circuit)" $ do
        -- Short-circuit OR: if e1 is true, return true without evaluating e2
        v1 <- irEval NotInTail e1
        labelTrue <- label "or.true"
        labelElse <- label "or.else"
        labelEnd <- label "or.end"
        br v1 [labelTrue, labelElse]

        -- True branch: short-circuit with true
        (labelTrueActual, _) <- block labelTrue $ do
          br1 labelEnd
          pure (I1 True)

        -- False branch: evaluate e2
        (labelElseActual, v2) <- block labelElse $ do
          v2 <- irEval NotInTail e2
          br1 labelEnd
          pure v2

        -- Merge results
        (_, result) <-
          block labelEnd $
            phi i1 [(labelTrueActual, I1 True), (labelElseActual, v2)]
        pure result
    OLtInt32 e1 e2 -> do
      irCommentBlock "OLtInt32" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        icmp SLt i1 v1 v2
    OLtInt64 e1 e2 -> do
      irCommentBlock "OLtInt64" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        icmp SLt i1 v1 v2
    OLtFloat e1 e2 -> do
      irCommentBlock "OLtFloat" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        fcmp OLt i1 v1 v2
    OLtDouble e1 e2 -> do
      irCommentBlock "OLtDouble" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        fcmp OLt i1 v1 v2
    OLteInt32 e1 e2 -> do
      irCommentBlock "OLteInt32" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        icmp SLte i1 v1 v2
    OLteInt64 e1 e2 -> do
      irCommentBlock "OLteInt64" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        icmp SLte i1 v1 v2
    OLteFloat e1 e2 -> do
      irCommentBlock "OLteFloat" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        fcmp OLte i1 v1 v2
    OLteDouble e1 e2 -> do
      irCommentBlock "OLteDouble" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        fcmp OLte i1 v1 v2
    OGtInt32 e1 e2 -> do
      irCommentBlock "OGtInt32" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        icmp SGt i1 v1 v2
    OGtInt64 e1 e2 -> do
      irCommentBlock "OGtInt64" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        icmp SGt i1 v1 v2
    OGtFloat e1 e2 -> do
      irCommentBlock "OGtFloat" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        fcmp OGt i1 v1 v2
    OGtDouble e1 e2 -> do
      irCommentBlock "OGtDouble" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        fcmp OGt i1 v1 v2
    OGteInt32 e1 e2 -> do
      irCommentBlock "OGteInt32" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        icmp SGte i1 v1 v2
    OGteInt64 e1 e2 -> do
      irCommentBlock "OGteInt64" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        icmp SGte i1 v1 v2
    OGteFloat e1 e2 -> do
      irCommentBlock "OGteFloat" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        fcmp OGte i1 v1 v2
    OGteDouble e1 e2 -> do
      irCommentBlock "OGteDouble" $ do
        v1 <- irEval NotInTail e1
        v2 <- irEval NotInTail e2
        fcmp OGte i1 v1 v2
    ONot e -> do
      irCommentBlock "ONot" $ do
        v1 <- irEval NotInTail e
        xor i1 v1 (I1 True)
    ONegFloat e -> do
      irCommentBlock "ONegFloat" $ do
        v1 <- irEval NotInTail e
        fneg TFloat v1
    ONegDouble e -> do
      irCommentBlock "ONegDouble" $ do
        v1 <- irEval NotInTail e
        fneg TDouble v1
