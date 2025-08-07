{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Pretty.Type (Pretty (..), renderPretty) where

import Coal.Common.List1 (NonEmpty (..), fromList1)
import Coal.Language
import Data.Text (Text, isPrefixOf)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

precArrow, precApp, precAtom :: Int
precArrow = 1 -- e.g., a -> b
precApp = 2 -- e.g., T f x y
precAtom = 3 -- variables, constructors, literals

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = id

tupledCompact :: [Doc ann] -> Doc ann
tupledCompact = encloseSep "(" ")" ", "

instance Pretty (TypeIndex k) where
  pretty (TypeIndex _ i) = "_" <> pretty i

instance Pretty (Parameter k) where
  pretty (Parameter _ name) = pretty name

instance (Pretty k, Pretty (o k)) => Pretty (Type o k) where
  pretty = prettyTypePrec 0

prettyTypePrec :: (Pretty k, Pretty (o k)) => Int -> Type o k -> Doc ann
prettyTypePrec prec =
  \case
    TArrow t1 t2 ->
      parensIf (prec > precArrow) $
        group (prettyTypePrec (precArrow + 1) t1 <+> "->" <+> prettyTypePrec precArrow t2)
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
          "type"
            <+> pretty name
              <> tupledCompact (map (prettyTypePrec 0) args)
            <+> "="
            <+> prettyTypePrec precArrow t

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
    IRecord t ->
      prettyT t
    IString ->
      "string"
    IUnit ->
      "unit"
    IVoid ->
      "void"

prettyRow :: (Pretty (o k)) => (t -> Doc ann) -> Row o k t -> Doc ann
prettyRow prettyT =
  \case
    RNil ->
      "{}"
    RVariable v ->
      pretty v
    row ->
      braces (fields row)
 where
  fields (RExtend name ty rest) =
    pretty name <+> ":" <+> prettyT ty <> fieldSep rest
   where
    fieldSep RNil = mempty
    fieldSep _ = "," <+> fields rest
  fields _ = mempty

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
        group (prettyKindPrec (precKArrow + 1) k1 <+> "->" <+> prettyKindPrec precKArrow k2)

renderPretty :: (Pretty a) => a -> Text
renderPretty p = renderStrict . layoutPretty defaultLayoutOptions $ pretty p
