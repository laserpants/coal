{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Kernel.Translate.Record (
  translateRecord,
  extractRow,
  makeRecord,
) where

import Coal.Compiler.Kernel.Translate.Type (translateType)
import Coal.Compiler.Stack (CompilerT)
import Coal.Kernel.Language.Expr (Clause (..), Expr (..), Label (..))
import qualified Coal.Kernel.Language.Type as Kernel
import qualified Coal.Kernel.Language.Type.Constructors as Kernel
import Coal.Kernel.Language.Type.HasType (HasType, typeOf)
import Coal.Language (Expression, IndexedType, Kind, Type)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Extras (Dictionary)

translateRecord ::
  (Monad m) =>
  (Expression a Kind IndexedType -> CompilerT a m (Expr Kernel.Type)) ->
  Type o k ->
  Dictionary (Expression a Kind IndexedType) ->
  Maybe (Expression a Kind IndexedType) ->
  CompilerT a m (Expr Kernel.Type)
translateRecord translate t d me = do
  exprs <- traverse translate d
  expr0 <- traverse translate me
  let e2 =
        case expr0 of
          Nothing ->
            ENil
          Just e1 ->
            let t1 = extractRow e1
             in ECase
                  t1
                  e1
                  ( Clause
                      (Label (Kernel.TCon "record" [t1]) "$Record" :| [Label t1 "$row"])
                      (EVar (Label t1 "$row"))
                      :| []
                  )
  pure $
    makeRecord
      (translateType t)
      (foldr (uncurry EExt) e2 (Map.toList exprs))

extractRow :: (HasType a) => a -> Kernel.Type
extractRow e =
  case typeOf e of
    Kernel.TCon _ [r] ->
      r
    _ ->
      error "Implementation error"

makeRecord :: Kernel.Type -> Expr Kernel.Type -> Expr Kernel.Type
makeRecord t e1 =
  EApp
    t
    (ECon (Label (Kernel.arrow t1 (Kernel.TCon "record" [t1])) "$Record"))
    (e1 :| [])
 where
  t1 = typeOf e1
