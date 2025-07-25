{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Coal.Kernel.LLVM.IREval.Expr (IREval (..)) where

import Control.Arrow ((>>>))
import Data.Fix (Fix (..))
import Data.Functor.Foldable (project)
import Extra (forM)
import Coal.Common.Label (Label (..))
import Coal.Common.List1 (fromList1)
import Coal.Kernel.LLVM.IREval
import Coal.Kernel.LLVM.IREval.Closure (irApplyClosure)
import Coal.Kernel.LLVM.IREval.Comment (irCommentBlock)
import Coal.Kernel.LLVM.IREval.Conceal (irConceal, irReveal)
import Coal.Kernel.LLVM.IREval.Expr.App (irEvalApp)
import Coal.Kernel.LLVM.IREval.Expr.Match (irEvalMatch)
import Coal.Kernel.LLVM.IREval.Expr.Op (irEvalOp)
import Coal.Kernel.LLVM.IREval.Expr.Var (irEvalVar)
import Coal.Kernel.LLVM.IRInstruction (ICmpCond (..))
import Coal.Kernel.LLVM.IRInstruction.TH
import Coal.Kernel.LLVM.IRType (IRTyped (..))
import Coal.Kernel.LLVM.IRType.Syntax (i1, i8Ptr, stringLiteral)
import Coal.Kernel.LLVM.IRValue (IRValue (..), irPrimValue)
import Coal.Kernel.Language.Type.Arrow (returnTypeOf)

import qualified Data.Text as Text
import qualified Coal.Kernel.Language as Core

instance IREval (Core.Expr Core.Type) where
  irEval =
    project
      >>> \case
        Core.EOp op ->
          irCommentBlock "EOp" $
            irEvalOp op
        Core.ELit (Core.PString str) -> do
          v1 <- makeString str
          bitcast v1 i8Ptr
        Core.ELit (Core.PBignum n) -> do
          p1 <- makeBignum n
          callg i8Ptr "bignum_init" [p1]
        Core.ELit prim ->
          pure (irPrimValue prim)
        Core.EVar (Label t var) ->
          irCommentBlock "EVar" $
            irEvalVar t var
        Core.ELet vs e1 ->
          irCommentBlock "ELet" $ do
            bound <- forM vs $
              \(Core.Binding (Label _ name) e) -> do
                v1 <- irEval e
                pure (name, v1)
            bind (fromList1 bound) (irEval e1)
        Core.EApp t e1 es ->
          irCommentBlock "EApp" $
            case e1 of
              Fix (Core.EVar var) ->
                irEvalApp t var es
              _ -> do
                v1 <- irEval e1
                irApplyClosure t v1 es
        Core.EIf e1 e2 e3 ->
          irCommentBlock "EIf" $ do
            labelThen <- label "then"
            labelElse <- label "else"
            labelExit <- label "exit"
            r1 <- irEval e1
            br r1 [labelThen, labelElse]
            thenBlock <- block labelThen $ do
              r <- irEval e2
              br1 labelExit
              pure r
            elseBlock <- block labelElse $ do
              r <- irEval e3
              br1 labelExit
              pure r
            (_, v) <-
              block labelExit $
                phi (irTypeOf e2) [thenBlock, elseBlock]
            pure v
        Core.ECall (Label _ ll) es e ->
          irCommentBlock "ECall" $ do
            rs <- traverse irEval es
            v1 <- ccall i8Ptr ll rs
            v2 <- irEval e
            case v2 of
              Local{} -> do
                a1 <- alloca i8Ptr (I32 1)
                a2 <- getelementptr1 i8Ptr a1 (I32 0)
                store v1 a2
                r <- callg i8Ptr "apply" [v2, I32 1, a1]
                irReveal r (irTypeOf (returnTypeOf e))
              _ ->
                error "Implementation error"
        Core.EMat t e1 cs ->
          irCommentBlock "EMat" $
            irEvalMatch t e1 cs
        Core.ENil ->
          irCommentBlock "ENil" $
            callg i8Ptr "hashmap_init" []
        Core.EExt field e1 e2 ->
          irCommentBlock "EExt" $ do
            k1 <- makeKey field
            t2 <- getelementptr (stringLiteral (Text.length field + 1)) k1 (I32 0) (I32 0)
            v1 <- irEval e1
            v2 <- irEval e2
            callg i8Ptr "hashmap_insert" [v2, t2, v1]
        Core.ESel (Core.Focus field (Label _ var) (Label _ r)) e1 e2 ->
          irCommentBlock "ESel" $ do
            k1 <- makeKey field
            t2 <- getelementptr (stringLiteral (Text.length field + 1)) k1 (I32 0) (I32 0)
            v1 <- irEval e1
            v2 <- callg i8Ptr "hashmap_lookup" [v1, t2]
            bind [(var, v2), (r, v1)] (irEval e2)
        Core.EMem e ->
          irCommentBlock "EMem" $ do
            p1 <- memoize
            r1 <- load i8Ptr p1
            r2 <- icmp Eq i1 r1 Null
            labelIsNull <- label "is_null"
            labelNotNull <- label "not_null"
            labelEnd <- label "end"
            br r2 [labelIsNull, labelNotNull]
            isNullBlock <- block labelIsNull $ do
              r3 <- irEval e
              r4 <- irConceal r3
              store r4 p1
              br1 labelEnd
              pure r3
            notNullBlock <- block labelNotNull $ do
              r5 <- irReveal r1 (irTypeOf e)
              br1 labelEnd
              pure r5
            (_, v) <-
              block labelEnd $
                phi (irTypeOf e) [isNullBlock, notNullBlock]
            pure v
        e ->
          error (show e)
