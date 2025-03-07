{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IREval.Expr.App (irEvalApp) where

import Data.Text (Text)
import Noll.Common.List1 (List1, NonEmpty (..), fromList1)
import Noll.Core.LLVM.IREncodable (irEncode)
import Noll.Core.LLVM.IREval (IREval (..), irEvalArgs)
import Noll.Core.LLVM.IREval.Closure (irApplyClosure, irPackClosure)
import Noll.Core.LLVM.IREval.Comment (irCommentBlock, irComments)
import Noll.Core.LLVM.IREval.Malloc (irMalloc)
import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType (IRType (..))
import Noll.Core.LLVM.IRType.Syntax (i32, i8Ptr, struct)
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Core.Language (Expr)
import Noll.Label (Label (..))
import Noll.Utils (Name, forM_, isConstructor)

import qualified Noll.Core.Language as Core

irEvalApp :: (IREval (Expr t)) => t -> Label t -> List1 (Expr t) -> IRInstr IRValue
irEvalApp t ll@(Label _ var) es
  | isConstructor var = do
      irComments (comment var)
      vs <- irEvalArgs es
      (i, t1) <- iDataConstr (struct (i32 : (i8Ptr <$ vs))) var
      v1 <- irMalloc t1
      v2 <- iGep t1 v1 (I32 0) (I32 0)
      iStore (I32 (fromIntegral i)) v2
      forM_ (zip vs [1 ..]) $ \(v, n) -> do
        v3 <- iGep t1 v1 (I32 0) (I32 n)
        iStore v v3
      iBitcast v1 i8Ptr
  | otherwise =
      irCommentBlock "Function application" $ do
        v <- iLookup var
        case v of
          Local{} ->
            irApplyClosure v es
          Global (TFun _ ts) _
            | length ts == length es -> do
                vs <- irEvalArgs es
                iCall i8Ptr v vs
            | length ts > length es -> do
                rs <- irEvalArgs es
                irPackClosure var (length ts - length rs) rs
            | otherwise ->
                case splitAt (length ts) (fromList1 es) of
                  (a : as, b : bs) -> do
                    r1 <- irEval (Core.app t (Core.var ll) (a :| as))
                    irApplyClosure r1 (b :| bs)
                  _ ->
                    error "TODO"
          _ ->
            error "TODO"

{-# INLINE comment #-}
comment :: Name -> [Text]
comment name =
  [ "Apply data constructor: " <> irEncode name
  , "----------------------- ^"
  ]
