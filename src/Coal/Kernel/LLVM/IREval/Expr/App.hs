{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.LLVM.IREval.Expr.App (irEvalApp) where

import Coal.Common.Label (Label (..))
import Coal.Common.List1 (List1, NonEmpty (..), fromList1)
import Coal.Kernel.LLVM.IREncodable (irEncode)
import Coal.Kernel.LLVM.IREval (IREval (..))
import Coal.Kernel.LLVM.IREval.Closure (irApplyClosure, irPackClosure)
import Coal.Kernel.LLVM.IREval.Comment (irComment, irCommentBlock)
import Coal.Kernel.LLVM.IREval.Conceal (irConcealArgs, irReveal)
import Coal.Kernel.LLVM.IREval.Malloc (irMalloc)
import Coal.Kernel.LLVM.IRInstruction (IRConstructor (..), IRInstr)
import Coal.Kernel.LLVM.IRInstruction.TH
import Coal.Kernel.LLVM.IRType (IRType (..), IRTyped (..))
import Coal.Kernel.LLVM.IRType.Syntax (i32, i8Ptr, struct)
import Coal.Kernel.LLVM.IRValue (IRValue (..))
import Coal.Kernel.Language (Expr)
import Control.Monad (unless)
import Data.Text (Text)
import Extra (Name, forM, forSM_, isConstructor)

import qualified Coal.Kernel.Language as Core

irEvalApp :: (IRTyped t, IREval (Expr t)) => t -> Label t -> List1 (Expr t) -> IRInstr IRValue
irEvalApp t ll@(Label _ var) es
  | isConstructor var = do
      irComment (comment1 var)
      vs <- irConcealArgs es
      IRConstructor i t1 <- makeConstructor (struct (i32 : (i8Ptr <$ vs))) var
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
                vs <- irConcealArgs es
                r1 <- call i8Ptr v vs
                r2 <- irReveal r1 (irTypeOf t)
                unless (r1 == r2) (irComment ["^ Reveal return value as " <> irEncode (irTypeOf t)])
                pure r2
            | length ts > length es -> do
                vs <- forM es irEval
                irPackClosure var (length ts - length vs) (fromList1 vs)
            | otherwise ->
                case splitAt (length ts) (fromList1 es) of
                  (a : as, b : bs) -> do
                    r1 <- irEval (Core.app t (Core.var ll) (a :| as))
                    irApplyClosure t r1 (b :| bs)
                  ([], b : bs) -> do
                    r1 <- irEval (Core.var ll)
                    irApplyClosure t r1 (b :| bs)
                  (_, []) ->
                    error "Implementation error"
          _ ->
            error "Implementation error"

{-# INLINE comment1 #-}
comment1 :: Name -> [Text]
comment1 name =
  [ "Apply data constructor: " <> irEncode name
  , "----------------------- ^"
  ]
