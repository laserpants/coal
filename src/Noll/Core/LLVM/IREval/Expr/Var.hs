{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IREval.Expr.Var (irEvalVar) where

import Data.Text (Text)
import Noll.Core.LLVM.IREncodable (irEncode)
import Noll.Core.LLVM.IREval.Closure (irPackClosure)
import Noll.Core.LLVM.IREval.Comment (irComments)
import Noll.Core.LLVM.IREval.Malloc (irMalloc)
import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType (IRType (..))
import Noll.Core.LLVM.IRType.Syntax (i32, i8Ptr, struct)
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Core.Language.Type.Arrow (arity)
import Noll.Utils (Name, isConstructor)

import qualified Noll.Core.Language as Core

irEvalVar :: Core.Type -> Name -> IRInstr IRValue
irEvalVar t name
  | isConstructor name = do
      irComments (comment1 name)
      (i, t1) <- iDataConstr (struct [i32]) name
      v1 <- irMalloc t1
      v2 <- iGep t1 v1 (I32 0) (I32 0)
      iStore (I32 (fromIntegral i)) v2
      iBitcast v1 i8Ptr
  | otherwise = do
      v <- iLookup name
      irComments (comment2 (irEncode v))
      case v of
        Global (TFun _ ts) _ ->
          if arity t == 0
            then
              -- Global constant
              iCallGlobal i8Ptr name []
            else
              irPackClosure name (length ts) []
        _ ->
          pure v

comment1 :: Name -> [Text]
comment1 name =
  [ "Data constructor: " <> name
  , "----------------- ^"
  ]

comment2 :: Name -> [Text]
comment2 name =
  [ "Name: " <> name
  , "----- ^"
  ]
