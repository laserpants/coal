{-# LANGUAGE LambdaCase #-}

module Noll.Compiler.Lowpass.TranslateDefinition (translateDefinition) where

import Data.Data (Data)
import Lang.Common.List1 (fromList1)
import Noll.Compiler.Lowpass.TranslateExpression (translateExpression, translatePattern)
import Noll.Language
import Noll.Module.Constant (Constant (..))
import Noll.Module.Definition
import Noll.Module.Function (Function (..))

import qualified Lang.Lowpass.Language as Lowpass

type LowpassObject = Lowpass.Object Lowpass.Type (Lowpass.Expr Lowpass.Type)

translateDefinition :: (Data a) => Definition a Kind IndexedType -> [LowpassObject]
translateDefinition =
  \case
    DAnnotation u d ->
      undefined
    DInstance name t ds ->
      undefined
    DType name ps cs ->
      undefined
    DFunction name (Function _ (With _ t) ps e) ->
      [Lowpass.OFunction name (translatePattern <$> fromList1 ps) (translateExpression e)]
    DConstant name (Constant _ With{} e) ->
      [Lowpass.OConstant name (translateExpression e)]
    DTrait name ts _ _ ->
      -- (Type Parameter ()) [(Name, Type Parameter ())]
      undefined
    DInstance name t ds ->
      undefined
