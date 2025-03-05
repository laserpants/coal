{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IRInstruction.Eval.Var (irEvalVar) where

import Noll.Core.LLVM.IREncodable (irEncode)
import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.Eval.Closure (irPackClosure)
import Noll.Core.LLVM.IRInstruction.Eval.Malloc (irMalloc)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType (IRType (..))
import Noll.Core.LLVM.IRType.Syntax (i32, i8Ptr, struct)
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Core.Language.Type.Syntax (arity)
import Noll.Utils (Name, isConstructor)

import qualified Noll.Core.Language as Core

irEvalVar :: Core.Type -> Name -> IRInstr IRValue
irEvalVar t var
  | isConstructor var = do
      iComment ""
      iComment ("Data constructor: " <> var)
      iComment "----------------- ^"
      iComment ""
      (i, t1) <- iDataConstr (struct [i32]) var
      v1 <- irMalloc t1
      v2 <- iGep t1 v1 (I32 0) (I32 0)
      iStore (I32 (fromIntegral i)) v2
      iBCast v1 i8Ptr
  | otherwise = do
      v <- iLookup var
      iComment ""
      iComment ("Name: " <> irEncode v)
      iComment "----- ^"
      iComment ""
      case v of
        Global (TFun _ us) _ ->
          if arity t == 0
            then -- Global constant
              iCallGlobal i8Ptr var []
            else
              irPackClosure var (length us) []
        _ ->
          pure v
