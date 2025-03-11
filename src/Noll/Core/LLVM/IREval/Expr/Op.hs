{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IREval.Expr.Op (irEvalOp) where

import Noll.Core.LLVM.IREval
import Noll.Core.LLVM.IREval.Comment (irCommentBlock)
import Noll.Core.LLVM.IREval.Conceal (irConceal, irRevealExpr)
import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType (IRTyped (..))
import Noll.Core.LLVM.IRType.Syntax (i1, i32, i64)
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Core.Language (Op, Typed)

import qualified Noll.Core.Language as Core

irEvalOp :: (Typed e, IRTyped e, IREval e) => Op e -> IRInstr IRValue
irEvalOp =
  \case
    Core.OAddInt32 e1 e2 -> do
      v3 <- irCommentBlock "OAddInt32" $ do
        v1 <- irRevealExpr e1
        v2 <- irRevealExpr e2
        iAdd i32 v1 v2
      irConceal v3
    Core.OAddInt64 e1 e2 -> do
      v3 <- irCommentBlock "OAddInt64" $ do
        v1 <- irRevealExpr e1
        v2 <- irRevealExpr e2
        iAdd i64 v1 v2
      irConceal v3
    Core.OSubInt32 e1 e2 -> do
      v3 <- irCommentBlock "OSubInt32" $ do
        v1 <- irRevealExpr e1
        v2 <- irRevealExpr e2
        iSub i32 v1 v2
      irConceal v3
    Core.OSubInt64 e1 e2 -> do
      v3 <- irCommentBlock "OSubInt64" $ do
        v1 <- irRevealExpr e1
        v2 <- irRevealExpr e2
        iSub i64 v1 v2
      irConceal v3
    Core.OMulInt32 e1 e2 -> do
      v3 <- irCommentBlock "OMulInt32" $ do
        v1 <- irRevealExpr e1
        v2 <- irRevealExpr e2
        iMul i32 v1 v2
      irConceal v3
    Core.OMulInt64 e1 e2 -> do
      v3 <- irCommentBlock "OMulInt64" $ do
        v1 <- irRevealExpr e1
        v2 <- irRevealExpr e2
        iMul i64 v1 v2
      irConceal v3
    Core.ODivInt32 e1 e2 -> do
      v3 <- irCommentBlock "ODivInt32" $ do
        v1 <- irRevealExpr e1
        v2 <- irRevealExpr e2
        iDiv i32 v1 v2
      irConceal v3
    Core.ODivInt64 e1 e2 -> do
      v3 <- irCommentBlock "ODivInt64" $ do
        v1 <- irRevealExpr e1
        v2 <- irRevealExpr e2
        iDiv i64 v1 v2
      irConceal v3
    Core.OEqInt32 e1 e2 -> do
      v3 <- irCommentBlock "OEqInt32" $ do
        v1 <- irRevealExpr e1
        v2 <- irRevealExpr e2
        iCmpEq i1 v1 v2
      irConceal v3
    Core.OEqInt64 e1 e2 -> do
      v3 <- irCommentBlock "OEqInt64" $ do
        v1 <- irRevealExpr e1
        v2 <- irRevealExpr e2
        iCmpEq i1 v1 v2
      irConceal v3
    Core.OAnd e1 e2 -> do
      v3 <- irCommentBlock "OAnd" $ do
        v1 <- irRevealExpr e1
        v2 <- irRevealExpr e2
        iAnd i1 v1 v2
      irConceal v3
    Core.OOr e1 e2 -> do
      v3 <- irCommentBlock "OOr" $ do
        v1 <- irRevealExpr e1
        v2 <- irRevealExpr e2
        iOr i1 v1 v2
      irConceal v3
    Core.OLtInt32 e1 e2 -> do
      v3 <- irCommentBlock "OLtInt32" $ do
        v1 <- irRevealExpr e1
        v2 <- irRevealExpr e2
        iCmpSLt i1 v1 v2
      irConceal v3
    Core.OLtInt64 e1 e2 -> do
      v3 <- irCommentBlock "OLtInt64" $ do
        v1 <- irRevealExpr e1
        v2 <- irRevealExpr e2
        iCmpSLt i1 v1 v2
      irConceal v3
    Core.ONot e -> do
      v2 <- irCommentBlock "ONot" $ do
        v1 <- irRevealExpr e
        iXOr i1 v1 (I1 True)
      irConceal v2
    Core.OLtEInt32 e1 e2 -> do
      v3 <- irCommentBlock "OLtEInt32" $ do
        v1 <- irRevealExpr e1
        v2 <- irRevealExpr e2
        iCmpSLE i1 v1 v2
      irConceal v3
    Core.OLtEInt64 e1 e2 -> do
      v3 <- irCommentBlock "OLtEInt64" $ do
        v1 <- irRevealExpr e1
        v2 <- irRevealExpr e2
        iCmpSLE i1 v1 v2
      irConceal v3
    Core.OGtInt32 e1 e2 -> do
      v3 <- irCommentBlock "OGtInt32" $ do
        v1 <- irRevealExpr e1
        v2 <- irRevealExpr e2
        iCmpSGt i1 v1 v2
      irConceal v3
    Core.OGtInt64 e1 e2 -> do
      v3 <- irCommentBlock "OGtInt64" $ do
        v1 <- irRevealExpr e1
        v2 <- irRevealExpr e2
        iCmpSGt i1 v1 v2
      irConceal v3
    Core.OGtEInt32 e1 e2 -> do
      v3 <- irCommentBlock "OGtEInt32" $ do
        v1 <- irRevealExpr e1
        v2 <- irRevealExpr e2
        iCmpSGE i1 v1 v2
      irConceal v3
    Core.OGtEInt64 e1 e2 -> do
      v3 <- irCommentBlock "OGtEInt64" $ do
        v1 <- irRevealExpr e1
        v2 <- irRevealExpr e2
        iCmpSGE i1 v1 v2
      irConceal v3
