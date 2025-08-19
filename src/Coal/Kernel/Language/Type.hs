{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Kernel.Language.Type (Type (..)) where

import Data.Data (Data, Typeable)
import qualified Data.Text as Text
import Extra (Name)
import Extra.Prettyprinter (parensIf)
import Prettyprinter

-- | Core language types
data Type
  = -- | Type constructor
    TCon Name [Type]
  | -- | Opaque type
    TOpq
  | -- | Row extension
    RExt Name Type Type
  | -- | Empty row
    RNil
  deriving (Show, Eq, Ord, Read, Data, Typeable)

precArrow, precApp :: Int
precArrow = 1
precApp = 2

tupledCompact :: [Doc ann] -> Doc ann
tupledCompact = encloseSep "(" ")" ", "

prettyTypePrec :: Int -> Type -> Doc ann
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

prettyRow :: Type -> Doc ann
prettyRow = braces . fields
 where
  fields =
    \case
      RExt f t1 r ->
        pretty f <+> ":" <+> pretty t1 <+> "|" <+> fields r
      RNil ->
        "{}"
      TOpq ->
        "*"
      x ->
        -- FIX:
        pretty (Text.replace "\"" "\\\"" (Text.pack (show x))) -- "??" -- error "Implementation error"

instance Pretty Type where
  pretty = prettyTypePrec 0
