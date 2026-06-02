{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Coal.Kernel.LLVM.IREval.Expr (IREval (..)) where

import Coal.Common.Label (Label (..))
import Coal.Kernel.LLVM.IREncodable (irEncode)
import Coal.Kernel.LLVM.IREval (IREval (..), IRTailContext (..))
import Coal.Kernel.LLVM.IREval.Closure (irApplyClosure, irPackClosure)
import Coal.Kernel.LLVM.IREval.Comment (irComment, irCommentBlock)
import Coal.Kernel.LLVM.IREval.Conceal (irConceal, irConcealArgs, irReveal)
import Coal.Kernel.LLVM.IREval.Expr.Match (irEvalMatch)
import Coal.Kernel.LLVM.IREval.Expr.Op (irEvalOp)
import Coal.Kernel.LLVM.IREval.Expr.Var (irEvalVar)
import Coal.Kernel.LLVM.IREval.Malloc (irMalloc)
import Coal.Kernel.LLVM.IRInstruction (ICmpCond (..), IRConstructor (..), IRInstr, TailMarker (..))
import Coal.Kernel.LLVM.IRInstruction.Builders
import Coal.Kernel.LLVM.IRType (IRType (..), IRTyped (..))
import Coal.Kernel.LLVM.IRType.Syntax (i1, i32, i8Ptr, stringLiteral, struct)
import Coal.Kernel.LLVM.IRValue (IRValue (..), irPrimValue)
import qualified Coal.Kernel.Language as Syntax
import Coal.Kernel.Language.Type.Arrow (arity, returnTypeOf)
import Control.Arrow ((>>>))
import Control.Monad (unless)
import Data.Fix (Fix (..))
import Data.Functor.Foldable (project)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Text (Text)
import qualified Data.Text as Text
import Extras (Name, forM, forSM_, isConstructor)

irEvalApp :: IRTailContext -> Syntax.Type -> Label Syntax.Type -> NonEmpty (Syntax.Expr Syntax.Type) -> IRInstr IRValue
irEvalApp tailCtx t ll@(Label vt var) es
  | isConstructor var = do
      irComment (comment1 var)
      vs <- irConcealArgs es
      IRConstructor i t1 <- makeConstructor (struct (i32 : replicate (arity vt) i8Ptr)) var
      v1 <- irMalloc t1
      v2 <- getelementptr t1 v1 (I32 0) (I32 0)
      store (I32 (fromIntegral i)) v2
      forSM_ 1 vs $
        \v n -> do
          v3 <- getelementptr t1 v1 (I32 0) (I32 n)
          store v v3
      bitcast v1 i8Ptr
  | otherwise =
      irCommentBlock "Function application" $ do
        v <- nameLookup var
        case v of
          Local{} ->
            irApplyClosure t v es
          Global (TFun _ ts) _
            | length ts == length es -> do
                -- Fully saturated call
                vs <- irConcealArgs es
                -- Check if this is a self-recursive tail call
                currentFn <- currentFunction
                let isSelfRecursive = currentFn == Just var
                    tailMarker = case (tailCtx, isSelfRecursive) of
                      (InTail, True) -> Tail -- Self-recursive tail call: use tail hint
                      (InTail, False) -> Tail -- Tail call to different function
                      _ -> NoTail -- Not in tail position
                r1 <- case tailMarker of
                  Tail -> tailCallg i8Ptr var vs
                  NoTail -> callg i8Ptr var vs
                  MustTail -> error "impossible: MustTail not used in this context"
                r2 <- irReveal r1 (irTypeOf t)
                unless (r1 == r2) (irComment ["^ Reveal return value as " <> irEncode (irTypeOf t)])
                pure r2
            | length ts > length es -> do
                vs <- forM es (irEval NotInTail) -- Arguments not in tail
                irPackClosure var (length ts - length vs) (toList vs)
            | otherwise ->
                case splitAt (length ts) (toList es) of
                  (a : as, b : bs) -> do
                    r1 <- irEval NotInTail (Syntax.app t (Syntax.var ll) (a :| as))
                    irApplyClosure t r1 (b :| bs)
                  ([], b : bs) -> do
                    r1 <- irEval NotInTail (Syntax.var ll)
                    irApplyClosure t r1 (b :| bs)
                  (_, []) ->
                    error "Implementation error: empty overapplication"
          _ ->
            error "Implementation error: non-function in application"

{-# INLINE comment1 #-}
comment1 :: Name -> [Text]
comment1 name =
  [ "Apply data constructor: " <> irEncode name
  , "----------------------- ^"
  ]

instance IREval (Syntax.Expr Syntax.Type) where
  irEval tailCtx =
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
          callg i8Ptr "coal_bignum_init" [p1]
        Syntax.ELit prim ->
          pure (irPrimValue prim)
        Syntax.EVar (Label t var) ->
          irCommentBlock "EVar" $
            irEvalVar t var
        Syntax.ELet vs e1 ->
          irCommentBlock "ELet" $ do
            bound <- forM vs $
              \(Syntax.Binding (Label _ name) e) -> do
                v1 <- irEval NotInTail e -- Bindings are not in tail position
                pure (name, v1)
            bind (toList bound) (irEval tailCtx e1) -- Body inherits tail context
        Syntax.EApp t e1 es ->
          irCommentBlock "EApp" $
            case e1 of
              Fix (Syntax.EVar var) ->
                irEvalApp tailCtx t var es -- Direct call, pass tail context
              _ -> do
                v1 <- irEval NotInTail e1 -- Function expression not in tail
                irApplyClosure t v1 es
        Syntax.EIf e1 e2 e3 ->
          irCommentBlock "EIf" $ do
            labelThen <- label "then"
            labelElse <- label "else"
            labelExit <- label "exit"
            r1 <- irEval NotInTail e1 -- Condition not in tail position
            br r1 [labelThen, labelElse]
            thenBlock <- block labelThen $ do
              r <- irEval tailCtx e2 -- Then branch inherits tail context
              br1 labelExit
              pure r
            elseBlock <- block labelElse $ do
              r <- irEval tailCtx e3 -- Else branch inherits tail context
              br1 labelExit
              pure r
            (_, v) <-
              block labelExit $
                phi (irTypeOf e2) [thenBlock, elseBlock]
            pure v
        Syntax.ECall (Label _ ll) es e ->
          irCommentBlock "ECall" $ do
            rs <- traverse (irEval NotInTail) es -- Arguments not in tail
            rsc <- traverse irConceal rs -- Conceal arguments for C ABI
            unless (rs == rsc) (irComment ["^ Conceal external call args"])
            v1 <- ccall i8Ptr ll rsc
            v2 <- irEval NotInTail e -- Continuation not in tail (C call)
            case v2 of
              Local{} -> do
                a1 <- alloca i8Ptr (I32 1)
                a2 <- getelementptr1 i8Ptr a1 (I32 0)
                store v1 a2
                r <- callg i8Ptr "apply" [v2, I32 1, a1]
                irReveal r (irTypeOf (returnTypeOf e))
              _ ->
                error "Implementation error: non-local value in apply"
        Syntax.EMat t e1 cs ->
          irCommentBlock "EMat" $
            irEvalMatch tailCtx t e1 cs -- Pass tail context to match
        Syntax.ENil ->
          irCommentBlock "ENil" $
            callg i8Ptr "rt_record_empty" []
        Syntax.EExt field e1 e2 ->
          irCommentBlock "EExt" $ do
            k1 <- makeKey field
            t2 <- getelementptr1 (stringLiteral (Text.length field + 1)) k1 (I32 0)
            v1 <- irEval NotInTail e1
            v2 <- irEval NotInTail e2
            v3 <- irConceal v1
            callg i8Ptr "rt_record_extend" [v2, t2, v3]
        Syntax.ESel (Syntax.Focus field (Label t var) (Label _ r)) e1 e2 ->
          irCommentBlock "ESel" $ do
            k1 <- makeKey field
            t2 <- getelementptr1 (stringLiteral (Text.length field + 1)) k1 (I32 0)
            v1 <- irEval NotInTail e1
            v2 <- callg i8Ptr "rt_record_lookup" [v1, t2]
            v3 <- irReveal v2 (irTypeOf t)
            bind [(var, v3), (r, v1)] (irEval tailCtx e2) -- Body inherits tail context
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
              r3 <- irEval tailCtx e -- Memoized value inherits tail context
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
          error ("Unsupported expression in IR generation: " <> show e)
