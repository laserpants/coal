{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Lowpass.TranslateDefinition where

import Noll.Language
import Noll.Module.Definition
import Noll.Module.Constant (Constant (..))
import Noll.Module.Function (Function (..))

import qualified Lang.Lowpass.Language as Lowpass

type LowpassObject = Lowpass.Object Lowpass.Type (Lowpass.Expr Lowpass.Type)

translateDefinition :: Definition a Kind IndexedType -> LowpassObject
translateDefinition =
  \case
    DAnnotation u d ->
      undefined
    DInstance name t ds ->
      undefined
    DType name ps cs ->
      undefined
    DFunction name (Function _ (With _ t) ps e) ->
      Lowpass.OFunction 
        undefined 
        undefined
        undefined
    DConstant name (Constant _ (With _ t) e) ->
      undefined
    DTrait name ts _ _ -> -- (Type Parameter ()) [(Name, Type Parameter ())]
      undefined
    DInstance name t ds ->
      undefined

--isFunction :: IndexedType -> Bool
--isFunction =
--  \case
--    TArrow{} ->
--      True
--    TAlias _ _ t ->
--      isFunction t
--    _ ->
--      False

--abc =
--  \case
--    EDictionaryLambda a ts e ->
--      undefined
--
--def =
--  \case
--    ELambda a ps e ->
--      undefined

