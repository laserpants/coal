{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Pretty.Kernel.Type where

import Coal.Kernel.Language.Type
import Coal.Pretty.Utils (parensIf, tupledCompact)
import Prettyprinter

import qualified Coal.Kernel.Language.Type as Kernel

precArrow, precApp :: Int
precArrow = 1
precApp = 2

prettyTypePrec :: Int -> Kernel.Type -> Doc ann
prettyTypePrec prec =
  \case
    TCon "/" [t1, t2] ->
      parensIf (prec > precArrow) $
        group (prettyTypePrec (precArrow + 1) t1 <> "/" <> prettyTypePrec precArrow t2)
    TCon "bool" [] ->
      "bool"
    TCon "int32" [] ->
      "int32"
    TCon "int64" [] ->
      "int64"
    TCon "float" [] ->
      "float"
    TCon "double" [] ->
      "double"
    TCon "char" [] ->
      "char"
    TCon "string" [] ->
      "string"
    TCon "bignum" [] ->
      "bignum"
    TCon "unit" [] ->
      "unit"
    TCon con ts ->
      parensIf (prec > precApp) $
        group (pretty con <> tupledCompact (map pretty ts))
    TOpq ->
      "*"
    r@RExt{} ->
      prettyRow r
    RNil ->
      "{}"

prettyRow :: Kernel.Type -> Doc ann
prettyRow = braces . fields
 where
  fields =
    \case
      RExt f t1 t2 ->
        pretty f <+> ":" <+> pretty t1 <+> "|" <+> fields t2
      RNil ->
        "{}"
      TOpq ->
        "*"
      _ ->
        error "Implementation error"

instance Pretty Kernel.Type where
  pretty = prettyTypePrec 0
