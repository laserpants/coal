{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IRInstruction.Eval (irEvalExpr) where

import Control.Arrow ((>>>))
import Control.Monad.Free (Free (..))
import Data.Fix (Fix (..))
import Data.Functor.Foldable (project)
import Data.Text (Text)
import Noll.Common.List1 (List1, fromList1)
import Noll.Core.LLVM.IRInstruction (IRInstr, IRInstrOpF (..))
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType (IRType (..), IRTyped (..), i1, i32, i64, i8, i8Ptr)
import Noll.Core.LLVM.IRValue (IRValue (..), irPrimValue)
import Noll.Label (Label (..), labelName)
import Noll.Utils (forM, isConstructor)

import qualified Data.Text as Text
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

{-# INLINE irEvalArgs #-}
irEvalArgs :: List1 CoreExpr -> IRInstr [IRValue]
irEvalArgs = mapM irEvalExpr . fromList1

irEvalOp =
  \case
    Core.OAddInt32 e1 e2 ->
      undefined
    Core.OAddInt64 e1 e2 ->
      undefined
    Core.OSubInt32 e1 e2 ->
      undefined
    Core.OSubInt64 e1 e2 ->
      undefined
    Core.OMulInt32 e1 e2 ->
      undefined
    Core.OMulInt64 e1 e2 ->
      undefined
    Core.ODivInt32 e1 e2 ->
      undefined
    Core.ODivInt64 e1 e2 ->
      undefined
    Core.OEqInt32 e1 e2 ->
      undefined
    Core.OEqInt64 e1 e2 ->
      undefined
    Core.OAnd e1 e2 -> do
      undefined
    Core.OOr e1 e2 -> do
      undefined
    Core.OLtInt32 e1 e2 -> do
      undefined
    Core.OLtInt64 e1 e2 -> do
      undefined
    Core.ONot e -> do
      undefined
    Core.OLtEInt32 e1 e2 -> do
      undefined
    Core.OLtEInt64 e1 e2 -> do
      undefined
    Core.OGtInt32 e1 e2 -> do
      undefined
    Core.OGtInt64 e1 e2 -> do
      undefined
    Core.OGtEInt32 e1 e2 -> do
      undefined
    Core.OGtEInt64 e1 e2 -> do
      undefined

irApplyClosure :: IRValue -> List1 CoreExpr -> IRInstr IRValue
irApplyClosure v es = do
  vs <- irEvalArgs es
  name <- iRuntimeApply (length es)
  iCallGlobal i8Ptr name (v : vs)

irEvalExpr :: CoreExpr -> IRInstr IRValue
irEvalExpr =
  project
    >>> \case
      Core.EOp op ->
        irEvalOp op
      Core.ELit Core.PChar{} ->
        error "TODO"
      Core.ELit Core.PString{} ->
        error "TODO"
      Core.ELit prim ->
        irConceal (irPrimValue prim)
      Core.EVar (Label t var)
        | isConstructor var -> do
            undefined
        | otherwise -> do
            undefined
      Core.ELet vs e1 ->
        irCommentBlock "ELet" $ do
          bound <- forM vs $ \(Core.Binding (Label _ name) e) -> do
            v <- optimizePtrConversions (irEvalExpr e)
            pure (name, v)
          iBind (fromList1 bound) (optimizePtrConversions (irEvalExpr e1))
      Core.EApp t e1@(Fix (Core.EVar (Label _ var))) es
        | isConstructor var -> do
            undefined
        | otherwise ->
            undefined
      Core.EApp t e1 es -> do
        v1 <- irEvalExpr e1
        irApplyClosure v1 es

optimizePtrConversions :: IRInstr IRValue -> IRInstr IRValue
optimizePtrConversions =
  \case
    Free (IInttoptr v1 t1 next) ->
      case next v1 of
        Free (IPtrtoint v2 t2 next1)
          | v1 == v2 ->
              next1 v1
        Free instr ->
          iInttoptr v1 t1 >>= optimizePtrConversions . next
        _ ->
          iInttoptr v1 t1 >>= next
    Free (IBind is i next) ->
      Free (IBind is (optimizePtrConversions i) (optimizePtrConversions <$> next))
    Free (IBlock name i next) ->
      Free (IBlock name (optimizePtrConversions i) (optimizePtrConversions <$> next))
    Free instr ->
      Free (optimizePtrConversions <$> instr)
    i ->
      i
