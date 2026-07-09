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
import qualified Coal.Kernel.Language.Type as NK
import qualified Coal.Kernel.Language.Type.Constructors as NKT
import Coal.Kernel.Language.Type.HasType (HasType, typeOf)
import Coal.Language (Expression, IndexedType, Kind, Type)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Extras (Dictionary)

translateRecord ::
  (Monad m) =>
  (Expression a Kind IndexedType -> CompilerT a m (Expr NK.Type)) ->
  Type o k ->
  Dictionary (Expression a Kind IndexedType) ->
  Maybe (Expression a Kind IndexedType) ->
  CompilerT a m (Expr NK.Type)
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
                      (Label (NK.TCon "record" [t1]) "$Record" :| [Label t1 "$row"])
                      (EVar (Label t1 "$row"))
                      :| []
                  )
  pure $
    makeRecord
      (translateType t)
      (foldr (uncurry EExt) e2 (Map.toList exprs))

extractRow :: (HasType a) => a -> NK.Type
extractRow e =
  case typeOf e of
    NK.TCon _ [r] ->
      r
    _ ->
      error "Implementation error"

makeRecord :: NK.Type -> Expr NK.Type -> Expr NK.Type
makeRecord t e1 =
  EApp
    t
    (ECon (Label (NKT.arrow t1 (NK.TCon "record" [t1])) "$Record"))
    (e1 :| [])
 where
  t1 = typeOf e1
