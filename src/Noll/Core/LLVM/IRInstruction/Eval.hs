{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IRInstruction.Eval (irEvalExpr) where

import Control.Arrow ((>>>))
import Control.Monad.Free (Free (..))
import Data.Fix (Fix (..))
import Data.Functor.Foldable (project)
import Data.Text (Text)
import Data.Tuple.Extra (fst3)
import Noll.Common.List1 (List1, NonEmpty (..), fromList1)
import Noll.Core.LLVM.IREncodable (irEncode)
import Noll.Core.LLVM.IRInstruction (IRInstr, IRInstrOpF (..))
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType (IRType (..), IRTyped (..))
import Noll.Core.LLVM.IRType.Syntax (
  i1,
  i32,
  i64,
  i8,
  i8Ptr,
  ptr,
  stringLiteralType,
  struct,
 )
import Noll.Core.LLVM.IRValue (IRValue (..), irPrimValue)
import Noll.Core.Language.Type.Syntax (arity)
import Noll.Label (Label (..))
import Noll.Utils (Name, forM, forM_, isConstructor)
import TextShow (showt)

import qualified Data.Text as Text
import qualified Noll.Common.List1 as List1
import qualified Noll.Core.Language as Core

type CoreExpr = Core.Expr Core.Type

irCommentBlock :: Text -> IRInstr a -> IRInstr a
irCommentBlock text block = do
  s <- iIndex
  iComment (Text.pack (replicate 75 '='))
  iComment ("[" <> s <> "] " <> text)
  iComment ""
  v <- block
  iComment ""
  iComment ("End: [" <> s <> "] " <> text)
  iComment "---- ^"
  pure v

irConceal :: IRValue -> IRInstr IRValue
irConceal v =
  case irTypeOf v of
    TInt1 ->
      iInttoptr v i8Ptr
    TInt8 ->
      iInttoptr v i8Ptr
    TInt32 ->
      iInttoptr v i8Ptr
    TInt64 ->
      iInttoptr v i8Ptr
    _ ->
      pure v

irReveal :: IRValue -> IRType -> IRInstr IRValue
irReveal v =
  \case
    TInt1 ->
      iPtrtoint v i1
    TInt8 ->
      iPtrtoint v i8
    TInt32 ->
      iPtrtoint v i32
    TInt64 ->
      iPtrtoint v i64
    _ ->
      pure v

irRevealExpr :: CoreExpr -> IRInstr IRValue
irRevealExpr expr = do
  v <- irEvalExpr expr
  irReveal v (irTypeOf (Core.typeOf expr))

{-# INLINE irEvalArgs #-}
irEvalArgs :: List1 CoreExpr -> IRInstr [IRValue]
irEvalArgs = mapM irEvalExpr . fromList1

irMalloc :: IRType -> IRInstr IRValue
irMalloc t = do
  irCommentBlock "gc_malloc" $ do
    r1 <- iGepNull (ptr t) (I32 1)
    r2 <- iPtrtoint r1 i64
    r3 <- iCallGlobal i8Ptr "gc_malloc" [r2]
    iBCast r3 (ptr t)

irClosureType :: Int -> IRType
irClosureType n = struct $ [i32] <> replicate (n + 3) i8Ptr

irPackClosure :: Name -> Int -> [IRValue] -> IRInstr IRValue
irPackClosure fn m vs = do
  (name, f1, f2, f3) <- iRuntimeClosure fn (length vs) m
  let t = TNamed name (irClosureType (length vs))
  r1 <- irMalloc t
  r2 <- iGep t r1 (I32 0) (I32 0)
  iComment ""
  iComment (showt (length vs) <> " argument(s) supplied -- target function '" <> fn <> "' expects " <> showt m <> " additional argument(s)")
  iComment ""
  r3 <- iInttoptr (I32 (fromIntegral m)) i8Ptr
  iStore r3 r2
  r4 <- iGep t r1 (I32 0) (I32 1)
  r5 <- iBCast f1 i8Ptr
  iStore r5 r4
  r6 <- iGep t r1 (I32 0) (I32 2)
  r7 <- iBCast f2 i8Ptr
  iStore r7 r6
  r8 <- iGep t r1 (I32 0) (I32 3)
  r9 <- iBCast f3 i8Ptr
  iStore r9 r8
  forM_ (zip vs [4 ..]) $ \(v, n) -> do
    qn <- iGep t r1 (I32 0) (I32 n)
    iStore v qn
  iBCast r1 i8Ptr

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
      error "TODO"
    Core.OLtEInt64 e1 e2 -> do
      error "TODO"
    Core.OGtInt32 e1 e2 -> do
      v3 <- irCommentBlock "OGtInt32" $ do
        v1 <- irRevealExpr e1
        v2 <- irRevealExpr e2
        iCmpSGt i1 v1 v2
      irConceal v3
    Core.OGtInt64 e1 e2 -> do
      error "TODO"
    Core.OGtEInt32 e1 e2 -> do
      error "TODO"
    Core.OGtEInt64 e1 e2 -> do
      error "TODO"
    _ ->
      error "TODO"

irEvalMatch :: CoreExpr -> List1 (Core.Clause Core.Type CoreExpr) -> IRInstr IRValue
irEvalMatch e1 cs = do
  v1 <- eliminatePtrConversions (irEvalExpr e1)
  r1 <- iBCast v1 (ptr (struct [i32]))
  r2 <- iGep (struct [i32]) r1 (I32 0) (I32 0)
  r3 <- iLoad i32 r2
  labelEnd <- iLabel "end"
  -- TODO: Handle case where length cs == 1
  ds <- forM cs $ \(Core.Clause ((Label _ con) :| lls) e) -> do
    n <- iLabel con
    pure (n, lls, e)
  let ns = fst3 <$> ds
  iSwitch r3 (List1.head ns) (fromList1 ns `zip` (I32 <$> [0 ..]))
  b :| bs <- forM ds $ \(ll, lls, e) ->
    iBlock ll $ do
      let s = struct (i32 : replicate (length lls) i8Ptr)
      r4 <- iBCast v1 (ptr s)
      bound <- forM (zip lls [1 ..]) $ \(Label _ n, i) -> do
        r5 <- iGep s r4 (I32 0) (I32 i)
        r6 <- iLoad i8Ptr r5
        pure (n, r6)
      r7 <- iBind bound (eliminatePtrConversions (irEvalExpr e))
      iBr1 labelEnd
      pure r7
  (_, v) <-
    iBlock labelEnd $
      iPhi (irTypeOf (snd b)) (b : bs)
  irConceal v

irEvalVar :: Core.Type -> Name -> IRInstr IRValue
irEvalVar t var
  | isConstructor var = do
      mapM_ iComment ["", "Data constructor: " <> var, "----------------- ^", ""]
      (i, t1) <- iDataConstr (struct [i32]) var
      v1 <- irMalloc t1
      v2 <- iGep t1 v1 (I32 0) (I32 0)
      iStore (I32 (fromIntegral i)) v2
      iBCast v1 i8Ptr
  | otherwise = do
      v <- iLookup var
      mapM_ iComment ["", "Name: " <> irEncode v, "----- ^", ""]
      case v of
        Global (TFun _ us) _ ->
          if arity t == 0
            then -- Global constant
              iCallGlobal i8Ptr var []
            else irPackClosure var (length us) []
        _ ->
          pure v

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
      Core.ECall (Label t ll) es e ->
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
