{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IRInstruction.Eval (irEvalExpr) where

import Control.Arrow ((>>>))
import Control.Monad.Free (Free (..))
import Data.Fix (Fix (..))
import Data.Functor.Foldable (project)
import Data.Tuple.Extra (fst3)
import Noll.Common.List1 (List1, NonEmpty (..), fromList1)
import Noll.Core.LLVM.IREncodable (irEncode)
import Noll.Core.LLVM.IRInstruction (IRInstr, IRInstrOpF (..))
import Noll.Core.LLVM.IRInstruction.Eval.Closure (irPackClosure)
import Noll.Core.LLVM.IRInstruction.Eval.CommentBlock (irCommentBlock)
import Noll.Core.LLVM.IRInstruction.Eval.Conceal (irConceal, irReveal)
import Noll.Core.LLVM.IRInstruction.Eval.Var (irEvalVar)
import Noll.Core.LLVM.IRInstruction.Eval.Malloc (irMalloc)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType (IRType (..), IRTyped (..))
import Noll.Core.LLVM.IRType.Syntax (
  i1,
  i32,
  i64,
  i8Ptr,
  ptr,
  stringLiteralType,
  struct,
 )
import Noll.Core.LLVM.IRValue (IRValue (..), irPrimValue)
import Noll.Label (Label (..))
import Noll.Utils (forM, forM_, isConstructor)

import qualified Noll.Core.Language as Core

type CoreExpr = Core.Expr Core.Type

irRevealExpr :: CoreExpr -> IRInstr IRValue
irRevealExpr expr = do
  v <- eliminatePtrConversions (irEvalExpr expr)
  irReveal v (irTypeOf (Core.typeOf expr))

{-# INLINE irEvalArgs #-}
irEvalArgs :: List1 CoreExpr -> IRInstr [IRValue]
irEvalArgs = mapM (eliminatePtrConversions . irEvalExpr) . fromList1

irApplyClosure :: IRValue -> List1 CoreExpr -> IRInstr IRValue
irApplyClosure v es = do
  vs <- irEvalArgs es
  name <- iRuntimeApply (length es)
  iCallGlobal i8Ptr name (v : vs)

irEvalOp :: Core.Op CoreExpr -> IRInstr IRValue
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
    _ ->
      error "TODO"

irEvalClause :: IRValue -> [Label Core.Type] -> CoreExpr -> IRInstr IRValue
irEvalClause v1 lls e = do
  r1 <- iBCast v1 (ptr s)
  bound <- forM (zip lls [1 ..]) $ \(Label _ n, i) -> do
    r2 <- iGep s r1 (I32 0) (I32 i)
    r3 <- iLoad i8Ptr r2
    pure (n, r3)
  iBind bound (eliminatePtrConversions (irEvalExpr e))
 where
  s = struct (i32 : replicate (length lls) i8Ptr)

irEvalMatch :: CoreExpr -> List1 (Core.Clause Core.Type CoreExpr) -> IRInstr IRValue
irEvalMatch e1 cs = do
  v1 <- eliminatePtrConversions (irEvalExpr e1)
  r1 <- iBCast v1 (ptr (struct [i32]))
  r2 <- iGep (struct [i32]) r1 (I32 0) (I32 0)
  r3 <- iLoad i32 r2
  labelEnd <- iLabel "end"
  case cs of
    Core.Clause ((Label _ _) :| lls) e :| [] ->
      irEvalClause v1 lls e
    _ -> do
      ds <- forM cs $ \(Core.Clause ((Label _ con) :| lls) e) -> do
        n <- iLabel con
        pure (n, lls, e)
      let n :| ns = fst3 <$> ds
      iSwitch r3 n (fromList1 (n :| ns) `zip` (I32 <$> [0 ..]))
      b :| bs <- forM ds $ \(ll, lls, e) ->
        iBlock ll $ do
          r4 <- irEvalClause v1 lls e
          iBr1 labelEnd
          pure r4
      (_, v) <-
        iBlock labelEnd $
          iPhi (irTypeOf (snd b)) (b : bs)
      irConceal v

irEvalApp :: Core.Type -> CoreExpr -> List1 CoreExpr -> IRInstr IRValue
irEvalApp t e1@(Fix (Core.EVar (Label _ var))) es
  | isConstructor var = do
      mapM_ iComment ["", "Apply data constructor: " <> irEncode var, "----------------------- ^", ""]
      vs <- irEvalArgs es
      (i, t1) <- iDataConstr (struct (i32 : (i8Ptr <$ vs))) var
      v1 <- irMalloc t1
      v2 <- iGep t1 v1 (I32 0) (I32 0)
      iStore (I32 (fromIntegral i)) v2
      forM_ (zip vs [1 ..]) $ \(v, n) -> do
        v3 <- iGep t1 v1 (I32 0) (I32 n)
        iStore v v3
      iBCast v1 i8Ptr
  | otherwise =
      irCommentBlock "Function application" $ do
        v <- iLookup var
        case v of
          Local{} ->
            irApplyClosure v es
          Global (TFun _ us) _
            | length us == length es -> do
                vs <- irEvalArgs es
                iCall i8Ptr v vs
            | length us > length es -> do
                rs <- irEvalArgs es
                irPackClosure var (length us - length rs) rs
            | otherwise ->
                case splitAt (length us) (fromList1 es) of
                  (a : as, b : bs) -> do
                    r1 <- eliminatePtrConversions (irEvalExpr (Core.app t e1 (a :| as)))
                    irApplyClosure r1 (b :| bs)
                  _ ->
                    error "TODO"
          _ ->
            error "TODO"
irEvalApp _ e1 es = do
  v1 <- irEvalExpr e1
  irApplyClosure v1 es

irEvalExpr :: CoreExpr -> IRInstr IRValue
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
            v <- eliminatePtrConversions (irEvalExpr e)
            pure (name, v)
          iBind (fromList1 bound) (eliminatePtrConversions (irEvalExpr e1))
      Core.EApp t e1 es ->
        irCommentBlock "EApp" $ do
          irEvalApp t e1 es
      Core.EIf e1 e2 e3 -> do
        irCommentBlock "EIf" $ do
          labelThen <- iLabel "then"
          labelElse <- iLabel "else"
          labelExit <- iLabel "exit"
          r1 <- eliminatePtrConversions (irRevealExpr e1)
          iBr r1 [labelThen, labelElse]
          thenBlock <- iBlock labelThen $ do
            r <- eliminatePtrConversions (irEvalExpr e2)
            iBr1 labelExit
            pure r
          elseBlock <- iBlock labelElse $ do
            r <- eliminatePtrConversions (irEvalExpr e3)
            iBr1 labelExit
            pure r
          (_, v) <-
            iBlock labelExit $
              iPhi i8Ptr [thenBlock, elseBlock]
          irConceal v
      Core.ECall (Label _ ll) es e ->
        irCommentBlock "ECall" $ do
          rs <- traverse (eliminatePtrConversions . irEvalExpr) es
          v1 <- iCallGlobal i8Ptr ll rs
          v2 <- eliminatePtrConversions (irEvalExpr e)
          case v2 of
            Local{} -> do
              name <- iRuntimeApply 1
              iCallGlobal i8Ptr name [v2, v1]
            _ ->
              error "TODO"
      Core.EMat _ e1 cs ->
        irCommentBlock "EMat" $ do
          irEvalMatch e1 cs
      Core.ENil ->
        irCommentBlock "ENil" $
          iCallGlobal i8Ptr "hashmap_init" []
      Core.EExt (Label _ field) e1 e2 -> do
        irCommentBlock "EExt" $ do
          k1 <- iHashMapKey field
          t2 <- iGep (stringLiteralType field) k1 (I32 0) (I32 0)
          v1 <- eliminatePtrConversions (irEvalExpr e1)
          v2 <- eliminatePtrConversions (irEvalExpr e2)
          iCallGlobal i8Ptr "hashmap_insert" [v2, t2, v1]
      Core.ESel (Core.Focus field (Label _ var) (Label _ r)) e1 e2 ->
        irCommentBlock "ESel" $ do
          k1 <- iHashMapKey field
          t2 <- iGep (stringLiteralType field) k1 (I32 0) (I32 0)
          v1 <- eliminatePtrConversions (irEvalExpr e1)
          v2 <- iCallGlobal i8Ptr "hashmap_lookup" [v1, t2]
          iBind [(var, v2), (r, v1)] (eliminatePtrConversions (irEvalExpr e2))
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
            r3 <- irEvalExpr e
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

eliminatePtrConversions :: IRInstr IRValue -> IRInstr IRValue
eliminatePtrConversions =
  \case
    Free (IInttoptr v1 t1 next) ->
      case next v1 of
        Free (IPtrtoint v2 _ next1)
          | v1 == v2 ->
              next1 v1
        Free{} ->
          iInttoptr v1 t1 >>= eliminatePtrConversions . next
        _ ->
          iInttoptr v1 t1 >>= next
    Free (IBind is i next) ->
      Free (IBind is (eliminatePtrConversions i) (eliminatePtrConversions <$> next))
    Free (IBlock name i next) ->
      Free (IBlock name (eliminatePtrConversions i) (eliminatePtrConversions <$> next))
    Free instr ->
      Free (eliminatePtrConversions <$> instr)
    i ->
      i
