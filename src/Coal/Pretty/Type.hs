{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Pretty.Type where

import Coal.Common.List1 (fromList1)
import Coal.Language
import Coal.Pretty.Utils (parensIf, tupledCompact)
import Data.Text (Text, isPrefixOf)
import Prettyprinter

precArrow, precApp, precAtom :: Int
precArrow = 1 -- e.g., a -> b
precApp = 2 -- e.g., T(x, y)
precAtom = 3 -- variables, constructors, literals

instance Pretty (TypeIndex k) where
  pretty (TypeIndex _ i) = pretty i

instance Pretty (Parameter k) where
  pretty (Parameter _ name) = pretty name

instance (Pretty k, Pretty (o k)) => Pretty (Type o k) where
  pretty = prettyTypePrec 0

prettyTypePrec :: (Pretty k, Pretty (o k)) => Int -> Type o k -> Doc ann
prettyTypePrec prec =
  \case
    TArrow t1 t2 ->
      parensIf (prec > precArrow) $
        group (prettyTypePrec (precArrow + 1) t1 <+> "→" <+> prettyTypePrec precArrow t2)
    TApplication _ (TConstructor _ con) args
      | "#Tuple" `isPrefixOf` con ->
          parensIf (prec > precApp) $ group (tupled (map (prettyTypePrec 0) (fromList1 args)))
    TApplication _ f args ->
      parensIf (prec > precApp) $
        group (prettyTypePrec precApp f <> tupledCompact (map (prettyTypePrec 0) (fromList1 args)))
    TConstructor _ name ->
      pretty name
    TVariable v ->
      pretty v
    TIntrinsic i ->
      prettyIntrinsic (prettyTypePrec precAtom) i
    TRow row ->
      prettyRow (prettyTypePrec precAtom) row
    TAlias name args t ->
      parensIf (prec > precApp) $
        group $
          "alias"
            <+> pretty name
              <> prettyArgs
            <+> "="
            <+> prettyTypePrec precArrow t
     where
      prettyArgs = if null args then "" else tupledCompact (map (prettyTypePrec 0) args)

prettyIntrinsic :: (t -> Doc ann) -> Intrinsic t -> Doc ann
prettyIntrinsic prettyT =
  \case
    IBool ->
      "bool"
    IChar ->
      "char"
    IDouble ->
      "double"
    IFloat ->
      "float"
    IInt32 ->
      "int32"
    IInt64 ->
      "int64"
    IBignum ->
      "bignum"
    INat ->
      "nat"
    IString ->
      "string"
    IUnit ->
      "unit"
    IVoid ->
      "void"
    IRecord t ->
      prettyT t

prettyRow :: (Pretty (o k)) => (t -> Doc ann) -> Row o k t -> Doc ann
prettyRow prettyT = braces . fields
 where
  fields (RExtend name ty rest) =
    pretty name <+> ":" <+> prettyT ty <> fieldSep rest
   where
    fieldSep RNil = mempty
    fieldSep _ = "," <+> fields rest
  fields (RVariable v) =
    pretty v
  fields RNil =
    "{}"

precKArrow :: Int
precKArrow = 1 -- a -> b

instance Pretty Kind where
  pretty = prettyKindPrec 0

prettyKindPrec :: Int -> Kind -> Doc ann
prettyKindPrec prec =
  \case
    KType ->
      "*"
    KRow ->
      "Row"
    KTrait ->
      "Trait"
    KArrow k1 k2 ->
      parensIf (prec > precKArrow) $
        group (prettyKindPrec (precKArrow + 1) k1 <+> "→" <+> prettyKindPrec precKArrow k2)

instance (Pretty t) => Pretty (Trait t) where
  pretty (Trait name t) =
    pretty name <> parens (pretty t)
