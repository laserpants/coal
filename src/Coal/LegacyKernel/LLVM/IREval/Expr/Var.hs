{-# LANGUAGE OverloadedStrings #-}

module Coal.LegacyKernel.LLVM.IREval.Expr.Var (irEvalVar) where

import Coal.LegacyKernel.LLVM.IREncodable (irEncode)
import Coal.LegacyKernel.LLVM.IREval.Closure (irPackClosure)
import Coal.LegacyKernel.LLVM.IREval.Comment (irComment)
import Coal.LegacyKernel.LLVM.IREval.Conceal (irConceal)
import Coal.LegacyKernel.LLVM.IREval.Malloc (irMalloc)
import Coal.LegacyKernel.LLVM.IRInstruction (IRConstructor (..), IRInstr)
import Coal.LegacyKernel.LLVM.IRInstruction.Builders
import Coal.LegacyKernel.LLVM.IRType (IRType (..))
import Coal.LegacyKernel.LLVM.IRType.Syntax (i32, i8Ptr, ptr, struct)
import Coal.LegacyKernel.LLVM.IRValue (IRValue (..))
import qualified Coal.LegacyKernel.Language as Syntax
import Coal.LegacyKernel.Language.Type.Arrow (arity)
import Data.Text (Text)
import Extras (Name, isConstructor)

irEvalVar :: Syntax.Type -> Name -> IRInstr IRValue
irEvalVar t name
  | isConstructor name = do
      irComment (comment1 name)
      IRConstructor i t1 <- makeConstructor (struct (i32 : replicate (arity t) i8Ptr)) name
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
            then callg i8Ptr name []
            else irPackClosure name (length ts) []
        Global t1 name1 | t == Syntax.string -> do
          v1 <- irConceal (Global t1 name1)
          bitcast v1 i8Ptr
        Global t1 name1 -> do
          v1 <- load t1 (Global (ptr t1) name1)
          irConceal v1
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
