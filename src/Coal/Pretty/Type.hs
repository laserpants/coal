{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Pretty.Type where

import Data.Text (Text)
import Prettyprinter.Render.Text (renderStrict)
import Coal.Common.List1 (NonEmpty (..), fromList1)
import Coal.Language
import Prettyprinter

precArrow, precApp, precAtom :: Int
precArrow = 1 -- e.g., a -> b
precApp = 2 -- e.g., T f x y
precAtom = 3 -- variables, constructors, literals

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = id

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
    TApplication _ f args ->
      parensIf (prec > precApp) $
        group (prettyTypePrec precApp f <> tupled (map (prettyTypePrec 0) (fromList1 args)))
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
          "type" <+> pretty name <> tupled (map (prettyTypePrec 0) args)
            <+> "=" <+> prettyTypePrec precArrow t

prettyIntrinsic :: (Type o k -> Doc ann) -> Intrinsic (Type o k) -> Doc ann
prettyIntrinsic go = 
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
    IList t ->
      group ("list" <> tupled [go t])
    INat ->
      "nat"
    IOption t ->
      group ("option" <> tupled [go t])
    IRecord t ->
      go t
    IResult t ->
      group ("result" <> tupled [go t])
    IString ->
      "string"
    ITuple ts ->
      group ("result" <> tupled (go <$> ts))
    IUnit ->
      "unit"
    IVoid ->
      "void"

prettyRow :: (Pretty (o k)) => (t -> Doc ann) -> Row o k t -> Doc ann
prettyRow prettyT =
  \case
    RNil ->
      "{}"
    (RVariable v) ->
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

-- Lower number = binds less tightly
precKArrow, precKAtom :: Int
precKArrow = 1  -- a -> b
precKAtom  = 2  -- base kinds like Type, Row, Trait

instance Pretty Kind where
  pretty = prettyKindPrec 0

prettyKindPrec :: Int -> Kind -> Doc ann
prettyKindPrec prec = 
  \case
    KType  -> 
      "*"
    KRow   -> 
      "Row"
    KTrait -> 
      "Trait"
    KArrow k1 k2 ->
      parensIf (prec > precKArrow) $
        group (prettyKindPrec (precKArrow + 1) k1 <+> "->" <+> prettyKindPrec precKArrow k2)

renderPretty :: (Pretty a) => a -> Text
renderPretty p = renderStrict . layoutPretty defaultLayoutOptions $ pretty p
