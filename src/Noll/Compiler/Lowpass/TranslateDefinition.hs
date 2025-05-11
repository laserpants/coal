{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Lowpass.TranslateDefinition (translateDefinition) where

import Data.Data (Data)
import Data.List.Extra (sortOn)
import Lang.Common.List1 (NonEmpty ((:|)), fromList1, (<|))
import Lang.Label (Label (..))
import Lang.Utils (Name)
import Noll.Compiler.Lowpass.TranslateExpression (translateExpression, translatePattern)
import Noll.Compiler.Lowpass.TranslateType (translateType)
import Noll.Language
import Noll.Language.Trait (With (..))
import Noll.Module.Constant (Constant (..))
import Noll.Module.Definition
import Noll.Module.Function (Function (..))
import Noll.Utils (hashed)

import qualified Lang.Lowpass.Language as Lowpass

type LowpassObject = Lowpass.Object Lowpass.Type (Lowpass.Expr Lowpass.Type)

translateDefinition :: (Data a) => Definition a Kind IndexedType -> [LowpassObject]
translateDefinition =
  \case
    DAnnotation _ d ->
      translateDefinition d
    DType _ _ cs ->
      translateConstructor <$> zip [0 ..] (sortOn constructorName cs)
    DFunction name (Function _ (With _ t) ps e) ->
      [Lowpass.OFunction name (translatePattern <$> fromList1 ps) (translateExpression e)]
    DConstant name (Constant _ With{} e) ->
      [Lowpass.OConstant name (translateExpression e)]
    DTrait name _ _ ins ->
      flip map ins $
        \(n, t) ->
          traitAccessor name n (translateType t)
    DInstance _ t ds ->
      flip concatMap ds $
        \case
          DFunction name f ->
            translateDefinition (DFunction (name <> postfix) f)
          DConstant name c ->
            translateDefinition (DConstant (name <> postfix) c)
     where
      postfix = "__$instance." <> hashed t
    _ ->
      []

traitAccessor :: Name -> Name -> Lowpass.Type -> LowpassObject
traitAccessor trait fn t =
  Lowpass.OFunction
    fn
    [dict]
    ( Lowpass.match
        t
        (Lowpass.var dict)
        ( Lowpass.Clause
            (Label (Lowpass.functionTypeOf dict [row]) "$Record" <| row :| [])
            ( Lowpass.sel
                (Lowpass.Focus fn var (Label Lowpass.opaque "_"))
                (Lowpass.var row)
                (Lowpass.var var)
            )
            :| []
        )
    )
 where
  var = Label t "$f"
  row = Label (Lowpass.RExt fn t Lowpass.opaque) "$r"
  dict = Label (Lowpass.TCon trait [Lowpass.opaque]) "$a"

translateConstructor :: (Int, Constructor Parameter () (Type Parameter ())) -> LowpassObject
translateConstructor (index, Constructor name _ (Forall _ _ t)) =
  Lowpass.OData name index (translateType t)
