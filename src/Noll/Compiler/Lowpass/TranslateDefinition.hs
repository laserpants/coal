{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Lowpass.TranslateDefinition (translateDefinition) where

import Data.Data (Data)
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
    --    DType _ _ cs ->
    --      translateConstructor <$> cs
    DFunction name (Function _ (With _ t) ps e) ->
      [Lowpass.OFunction name (translatePattern <$> fromList1 ps) (translateExpression e)]
    DConstant name (Constant _ With{} e) ->
      [Lowpass.OConstant name (translateExpression e)]
    DTrait name ts ps ins ->
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
    [Label d "$a"]
    ( Lowpass.match
        (Lowpass.returnTypeOf t)
        (Lowpass.var (Label d "$a"))
        ( Lowpass.Clause
            (Label (r `Lowpass.arrow` d) "$Record" <| Label r "$r" :| [])
            ( Lowpass.sel
                (Lowpass.Focus fn (Label t "$f") (Label Lowpass.opaque "_"))
                (Lowpass.var (Label r "$r"))
                (Lowpass.var (Label t "$f"))
            )
            :| []
        )
    )
 where
  d = Lowpass.TCon trait [Lowpass.opaque]
  r = Lowpass.RExt fn t Lowpass.opaque

-- TODO
translateConstructor :: Constructor Parameter () (Type Parameter ()) -> LowpassObject
translateConstructor (Constructor name _ (Forall _ _ _)) =
  Lowpass.OData name undefined undefined
