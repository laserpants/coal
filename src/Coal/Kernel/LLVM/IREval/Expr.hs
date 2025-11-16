{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Coal.Kernel.LLVM.IREval.Expr (IREval (..)) where

import Coal.Common.Label (Label (..))
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
import qualified Coal.Kernel.Language as Syntax
import Coal.Kernel.Language.Type.Arrow (returnTypeOf)
import Control.Arrow ((>>>))
import Data.Fix (Fix (..))
import Data.Functor.Foldable (project)
import Data.List.NonEmpty (toList)
import qualified Data.Text as Text
import Extras (forM)

instance IREval (Syntax.Expr Syntax.Type) where
  irEval =
    project
      >>> \case
        Syntax.EOp op ->
          irCommentBlock "EOp" $
            irEvalOp op
        Syntax.ELit (Syntax.PString str) -> do
          v1 <- makeString str
          bitcast v1 i8Ptr
        Syntax.ELit (Syntax.PBignum n) -> do
          p1 <- makeBignum n
          callg i8Ptr "bignum_init" [p1]
        Syntax.ELit prim ->
          pure (irPrimValue prim)
        Syntax.EVar (Label t var) ->
          irCommentBlock "EVar" $
            irEvalVar t var
        Syntax.ELet vs e1 ->
          irCommentBlock "ELet" $ do
            bound <- forM vs $
              \(Syntax.Binding (Label _ name) e) -> do
                v1 <- irEval e
                pure (name, v1)
            bind (toList bound) (irEval e1)
        Syntax.EApp t e1 es ->
          irCommentBlock "EApp" $
            case e1 of
              Fix (Syntax.EVar var) ->
                irEvalApp t var es
              _ -> do
                v1 <- irEval e1
                irApplyClosure t v1 es
        Syntax.EIf e1 e2 e3 ->
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
        Syntax.ECall (Label _ ll) es e ->
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
        Syntax.EMat t e1 cs ->
          irCommentBlock "EMat" $
            irEvalMatch t e1 cs
        Syntax.ENil ->
          irCommentBlock "ENil" $
            callg i8Ptr "hashmap_init" []
        Syntax.EExt field e1 e2 ->
          irCommentBlock "EExt" $ do
            k1 <- makeKey field
            t2 <- getelementptr (stringLiteral (Text.length field + 1)) k1 (I32 0) (I32 0)
            v1 <- irEval e1
            v2 <- irEval e2
            callg i8Ptr "hashmap_insert" [v2, t2, v1]
        Syntax.ESel (Syntax.Focus field (Label t var) (Label _ r)) e1 e2 ->
          irCommentBlock "ESel" $ do
            k1 <- makeKey field
            t2 <- getelementptr (stringLiteral (Text.length field + 1)) k1 (I32 0) (I32 0)
            v1 <- irEval e1
            v2 <- callg i8Ptr "hashmap_lookup" [v1, t2]
            v3 <- irReveal v2 (irTypeOf t)
            bind [(var, v3), (r, v1)] (irEval e2)
        Syntax.EMem e ->
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
