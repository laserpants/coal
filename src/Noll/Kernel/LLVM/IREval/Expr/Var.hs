{-# LANGUAGE OverloadedStrings #-}

module Noll.Kernel.LLVM.IREval.Expr.Var (irEvalVar) where

import Data.Text (Text)
import Noll.Kernel.LLVM.IREncodable (irEncode)
import Noll.Kernel.LLVM.IREval.Closure (irPackClosure)
import Noll.Kernel.LLVM.IREval.Comment (irComment)
import Noll.Kernel.LLVM.IREval.Conceal (irConceal)
import Noll.Kernel.LLVM.IREval.Malloc (irMalloc)
import Noll.Kernel.LLVM.IRInstruction (IRConstructor (..), IRInstr)
import Noll.Kernel.LLVM.IRInstruction.TH
import Noll.Kernel.LLVM.IRType (IRType (..))
import Noll.Kernel.LLVM.IRType.Syntax (i32, i8Ptr, struct)
import Noll.Kernel.LLVM.IRValue (IRValue (..))
import Noll.Kernel.Language.Type.Arrow (arity)
import Extra (Name, isConstructor)

import qualified Noll.Kernel.Language as Core

irEvalVar :: Core.Type -> Name -> IRInstr IRValue
irEvalVar t name
  | isConstructor name = do
      irComment (comment1 name)
      IRConstructor i t1 <- makeConstructor (struct [i32]) name
      v1 <- irMalloc t1
      v2 <- getelementptr t1 v1 (I32 0) (I32 0)
      store (I32 (fromIntegral i)) v2
      bitcast v1 i8Ptr
  | otherwise = do
      v <- nameLookup name
      irComment (comment2 name (irEncode v))
      case v of
        Global (TFun _ ts) _ ->
          if arity t == 0
            then
              callg i8Ptr name []
            else
              irPackClosure name (length ts) []
        Global t1 name1 | t == Core.string -> do
          v1 <- irConceal (Global t1 name1)
          bitcast v1 i8Ptr
        Global t1 name1 -> do
          irConceal (Global t1 name1)
        _ ->
          pure v

comment1 :: Name -> [Text]
comment1 name =
  [ "Data constructor: " <> name
  , "----------------- ^"
  ]

comment2 :: Name -> Name -> [Text]
comment2 name val =
  [ "Name: " <> name <> ", " <> val
  , "----- ^"
  ]
