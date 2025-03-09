{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IREval.Expr (irEvalExpr) where

import Control.Arrow ((>>>))
import Data.Fix (Fix (..))
import Data.Functor.Foldable (project)
import Noll.Common.List1 (fromList1)
import Noll.Core.LLVM.IREval
import Noll.Core.LLVM.IREval.Closure (irApplyClosure)
import Noll.Core.LLVM.IREval.Comment (irCommentBlock)
import Noll.Core.LLVM.IREval.Conceal (irConceal, irRevealExpr)
import Noll.Core.LLVM.IREval.Expr.App (irEvalApp)
import Noll.Core.LLVM.IREval.Expr.Match (irEvalMatch)
import Noll.Core.LLVM.IREval.Expr.Op (irEvalOp)
import Noll.Core.LLVM.IREval.Expr.Var (irEvalVar)
import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType.Syntax (i1, i8Ptr, stringLiteral)
import Noll.Core.LLVM.IRValue (IRValue (..), irPrimValue)
import Noll.Label (Label (..))
import Noll.Utils (forM)

import qualified Data.Text as Text
import qualified Noll.Core.Language as Core

instance IREval (Core.Expr Core.Type) where
  irEval = irEvalExpr

irEvalExpr :: Core.Expr Core.Type -> IRInstr IRValue
irEvalExpr =
  project
    >>> \case
      Core.EOp op ->
        irCommentBlock "EOp" $ do
          irEvalOp op
      Core.ELit Core.PString{} ->
        error "TODO"
      Core.ELit prim ->
        irConceal (irPrimValue prim)
      Core.EVar (Label t var) ->
        irCommentBlock "EVar" $ do
          irEvalVar t var
      Core.ELet vs e1 ->
        irCommentBlock "ELet" $ do
          bound <- forM vs $
            \(Core.Binding (Label _ name) e) -> do
              v <- irEval e
              pure (name, v)
          metaBind (fromList1 bound) (irEval e1)
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
          labelThen <- metaLabel "then"
          labelElse <- metaLabel "else"
          labelExit <- metaLabel "exit"
          r1 <- irRevealExpr e1
          iBr r1 [labelThen, labelElse]
          thenBlock <- metaBlock labelThen $ do
            r <- irEval e2
            iBr1 labelExit
            pure r
          elseBlock <- metaBlock labelElse $ do
            r <- irEval e3
            iBr1 labelExit
            pure r
          (_, v) <-
            metaBlock labelExit $
              iPhi i8Ptr [thenBlock, elseBlock]
          irConceal v
      Core.ECall (Label _ ll) es e ->
        irCommentBlock "ECall" $ do
          rs <- traverse irEval es
          v1 <- iCallGlobal i8Ptr ll rs
          v2 <- irEval e
          case v2 of
            Local{} -> do
              name <- metaApply 1
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
          k1 <- metaKey field
          t2 <- iGep (stringLiteral (Text.length field + 1)) k1 (I32 0) (I32 0)
          v1 <- irEval e1
          v2 <- irEval e2
          iCallGlobal i8Ptr "hashmap_insert" [v2, t2, v1]
      Core.ESel (Core.Focus field (Label _ var) (Label _ r)) e1 e2 ->
        irCommentBlock "ESel" $ do
          k1 <- metaKey field
          t2 <- iGep (stringLiteral (Text.length field + 1)) k1 (I32 0) (I32 0)
          v1 <- irEval e1
          v2 <- iCallGlobal i8Ptr "hashmap_lookup" [v1, t2]
          metaBind [(var, v2), (r, v1)] (irEval e2)
      Core.EMem e ->
        irCommentBlock "EMem" $ do
          p1 <- metaMemoize
          r1 <- iLoad i8Ptr p1
          r2 <- iCmpEq i1 r1 Null
          labelIsNull <- metaLabel "is_null"
          labelNotNull <- metaLabel "not_null"
          labelEnd <- metaLabel "end"
          iBr r2 [labelIsNull, labelNotNull]
          isNullBlock <- metaBlock labelIsNull $ do
            r3 <- irEval e
            iStore r3 p1
            iBr1 labelEnd
            pure r3
          notNullBlock <- metaBlock labelNotNull $ do
            iBr1 labelEnd
            pure r1
          (_, v) <-
            metaBlock labelEnd $
              iPhi i8Ptr [isNullBlock, notNullBlock]
          irConceal v
      e ->
        error (show e)
