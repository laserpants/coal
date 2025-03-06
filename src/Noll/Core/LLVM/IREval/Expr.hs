{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IREval.Expr (irEvalExpr) where

import Control.Arrow ((>>>))
import Control.Monad.Free (Free (..))
import Data.Fix (Fix (..))
import Data.Functor.Foldable (project)
import Noll.Common.List1 (fromList1)
import Noll.Core.LLVM.IREval
import Noll.Core.LLVM.IREval.Expr.App (irEvalApp)
import Noll.Core.LLVM.IREval.Expr.Match (irEvalMatch)
import Noll.Core.LLVM.IREval.Expr.Op (irEvalOp)
import Noll.Core.LLVM.IREval.Expr.Var (irEvalVar)
import Noll.Core.LLVM.IRInstruction (IRInstr, IRInstrOpF (..))
import Noll.Core.LLVM.IRInstruction.Eval.Closure (irApplyClosure)
import Noll.Core.LLVM.IRInstruction.Eval.Comment (irCommentBlock)
import Noll.Core.LLVM.IRInstruction.Eval.Conceal (irConceal, irRevealExpr)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType.Syntax (i1, i8Ptr, stringLiteralType)
import Noll.Core.LLVM.IRValue (IRValue (..), irPrimValue)
import Noll.Label (Label (..))
import Noll.Utils (forM)

import qualified Noll.Core.Language as Core

instance IREval (Core.Expr Core.Type) where
  irEval = simplify . irEvalExpr

irEvalExpr :: Core.Expr Core.Type -> IRInstr IRValue
irEvalExpr =
  project
    >>> \case
      Core.EOp op ->
        irCommentBlock "EOp" $ do
          irEvalOp op
      Core.ELit Core.PChar{} ->
        error "TODO"
      Core.ELit Core.PString{} ->
        error "TODO"
      Core.ELit prim ->
        irConceal (irPrimValue prim)
      Core.EVar (Label t var) ->
        irCommentBlock "EVar" $ do
          irEvalVar t var
      Core.ELet vs e1 ->
        irCommentBlock "ELet" $ do
          bound <- forM vs $ \(Core.Binding (Label _ name) e) -> do
            v <- irEval e
            pure (name, v)
          iBind (fromList1 bound) (irEval e1)
      Core.EApp t e1 es ->
        irCommentBlock "EApp" $ do
          case e1 of
            Fix (Core.EVar var) ->
              irEvalApp t var es
            _ -> do
              v1 <- irEval e1
              irApplyClosure v1 es
      Core.EIf e1 e2 e3 -> do
        irCommentBlock "EIf" $ do
          labelThen <- iLabel "then"
          labelElse <- iLabel "else"
          labelExit <- iLabel "exit"
          r1 <- irRevealExpr e1
          iBr r1 [labelThen, labelElse]
          thenBlock <- iBlock labelThen $ do
            r <- irEval e2
            iBr1 labelExit
            pure r
          elseBlock <- iBlock labelElse $ do
            r <- irEval e3
            iBr1 labelExit
            pure r
          (_, v) <-
            iBlock labelExit $
              iPhi i8Ptr [thenBlock, elseBlock]
          irConceal v
      Core.ECall (Label _ ll) es e ->
        irCommentBlock "ECall" $ do
          rs <- traverse irEval es
          v1 <- iCallGlobal i8Ptr ll rs
          v2 <- irEval e
          case v2 of
            Local{} -> do
              name <- iApply 1
              iCallGlobal i8Ptr name [v2, v1]
            _ ->
              error "TODO"
      Core.EMat _ e1 cs ->
        irCommentBlock "EMat" $ do
          irEvalMatch e1 cs
      Core.ENil ->
        irCommentBlock "ENil" $ do
          iCallGlobal i8Ptr "hashmap_init" []
      Core.EExt (Label _ field) e1 e2 -> do
        irCommentBlock "EExt" $ do
          k1 <- iHashMapKey field
          t2 <- iGep (stringLiteralType field) k1 (I32 0) (I32 0)
          v1 <- irEval e1
          v2 <- irEval e2
          iCallGlobal i8Ptr "hashmap_insert" [v2, t2, v1]
      Core.ESel (Core.Focus field (Label _ var) (Label _ r)) e1 e2 ->
        irCommentBlock "ESel" $ do
          k1 <- iHashMapKey field
          t2 <- iGep (stringLiteralType field) k1 (I32 0) (I32 0)
          v1 <- irEval e1
          v2 <- iCallGlobal i8Ptr "hashmap_lookup" [v1, t2]
          iBind [(var, v2), (r, v1)] (irEval e2)
      Core.EMem e ->
        irCommentBlock "EMem" $ do
          p1 <- iLabel "ptr"
          r1 <- iLoad i8Ptr (Global i8Ptr p1)
          r2 <- iCmpEq i1 r1 Null
          labelIsNull <- iLabel "is_null"
          labelNotNull <- iLabel "not_null"
          labelEnd <- iLabel "end"
          iBr r2 [labelIsNull, labelNotNull]
          isNullBlock <- iBlock labelIsNull $ do
            r3 <- irEval e
            iStore r3 (Global i8Ptr p1)
            iBr1 labelEnd
            pure r3
          notNullBlock <- iBlock labelNotNull $ do
            iBr1 labelEnd
            pure r1
          (_, v) <-
            iBlock labelEnd $
              iPhi i8Ptr [isNullBlock, notNullBlock]
          irConceal v
      e ->
        error (show e)

simplify :: IRInstr IRValue -> IRInstr IRValue
simplify =
  \case
    Free (IInttoptr v1 t1 next) ->
      case next v1 of
        Free (IPtrtoint v2 _ next1)
          | v1 == v2 ->
              next1 v1
        Free{} ->
          iInttoptr v1 t1 >>= simplify . next
        _ ->
          iInttoptr v1 t1 >>= next
    Free (IBind is i next) ->
      Free (IBind is (simplify i) (simplify <$> next))
    Free (IBlock name i next) ->
      Free (IBlock name (simplify i) (simplify <$> next))
    Free instr ->
      Free (simplify <$> instr)
    i ->
      i
