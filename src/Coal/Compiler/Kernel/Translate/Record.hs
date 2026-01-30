{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Kernel.Translate.Record (
  translateRecord,
  extractRow,
  makeRecord,
) where

import Coal.Common.Label (Label (..))
import Coal.Compiler.Kernel.Translate.Type (translateType)
import Coal.Compiler.Stack (CompilerT)
import Coal.Kernel.Compiler (KernelExpr)
import qualified Coal.Kernel.Language as Kernel
import Coal.Language (Expression, IndexedType, Type)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Extras (Dictionary)

translateRecord :: (Monad m) => (Expression a () IndexedType -> CompilerT a m KernelExpr) -> Type o k -> Dictionary (Expression a () IndexedType) -> Maybe (Expression a () IndexedType) -> CompilerT a m KernelExpr
translateRecord translate t d me = do
  exprs <- traverse translate d
  expr0 <- traverse translate me
  let e2 =
        case expr0 of
          Nothing ->
            Kernel.nil
          Just e1 -> do
            let t1 = extractRow e1
            Kernel.match
              t1
              e1
              ( Kernel.Clause
                  (Label (Kernel.record t1) "$Record" :| [Label t1 "$row"])
                  (Kernel.var (Label t1 "$row"))
                  :| []
              )
  pure $
    makeRecord
      (translateType t)
      (foldr (uncurry Kernel.ext) e2 (Map.toList exprs))

extractRow :: (Kernel.Typed a) => a -> Kernel.Type
extractRow e =
  case Kernel.typeOf e of
    Kernel.TCon _ [r] ->
      r
    _ ->
      error "Implementation error"

makeRecord :: Kernel.Type -> KernelExpr -> KernelExpr
makeRecord t e1 =
  Kernel.app
    t
    (Kernel.var (Label (Kernel.arrow t1 (Kernel.record t1)) "$Record"))
    (e1 :| [])
 where
  t1 = Kernel.typeOf e1
