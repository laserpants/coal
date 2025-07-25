{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Kernel.LLVM.IREval.Expr.App (irEvalApp) where

import Control.Monad (unless)
import Data.Text (Text)
import Noll.Common.List1 (List1, NonEmpty (..), fromList1)
import Noll.Common.Label (Label (..))
import Noll.Kernel.LLVM.IREncodable (irEncode)
import Noll.Kernel.LLVM.IREval (IREval (..))
import Noll.Kernel.LLVM.IREval.Closure (irApplyClosure, irPackClosure)
import Noll.Kernel.LLVM.IREval.Comment (irComment, irCommentBlock)
import Noll.Kernel.LLVM.IREval.Conceal (irConcealArgs, irReveal)
import Noll.Kernel.LLVM.IREval.Malloc (irMalloc)
import Noll.Kernel.LLVM.IRInstruction (IRConstructor (..), IRInstr)
import Noll.Kernel.LLVM.IRInstruction.TH
import Noll.Kernel.LLVM.IRType (IRType (..), IRTyped (..))
import Noll.Kernel.LLVM.IRType.Syntax (i32, i8Ptr, struct)
import Noll.Kernel.LLVM.IRValue (IRValue (..))
import Noll.Kernel.Language (Expr)
import Extra (Name, forM, forSM_, isConstructor)

import qualified Noll.Kernel.Language as Core

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
