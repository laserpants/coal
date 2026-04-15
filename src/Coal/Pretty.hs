{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Coal.Pretty (
  CoalPretty (..),
  Doc,
  Pretty (..),
  (<+>),
  vsep,
  hsep,
  vcat,
  hcat,
  sep,
  cat,
  fillSep,
  fillCat,
  enclose,
  encloseSep,
  parens,
  brackets,
  braces,
  angles,
  squotes,
  dquotes,
  group,
  nest,
  hang,
  indent,
  align,
  line,
  line',
  softline,
  softline',
  hardline,
  tupled,
  parensIf,
  Prec,
  precArrow,
  precApp,
  precAtom,
)
where

import Coal.Language.Trait (Trait (..))
import Coal.Language.Type (
  Parameter (..),
  Type (..),
  TypeIndex (..),
  isTupleType,
  typeArgs,
 )
import Coal.Language.Type.Intrinsic (Intrinsic (..))
import Coal.Language.Type.Kind (Kind (..))
import Coal.Language.Type.Row (Row (..))
import Coal.Language.Type.Scheme (Scheme (..))
import Coal.Utils (intToVar)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Extras.Prettyprinter (Prec, parensIf, precApp, precArrow, precAtom, typeBrackets)
import Prettyprinter (
  Doc,
  Pretty (..),
  align,
  angles,
  braces,
  brackets,
  cat,
  dquotes,
  enclose,
  encloseSep,
  fillCat,
  fillSep,
  group,
  hang,
  hardline,
  hcat,
  hsep,
  indent,
  line,
  line',
  nest,
  parens,
  sep,
  softline,
  softline',
  space,
  squotes,
  tupled,
  vcat,
  vsep,
  (<+>),
 )

class CoalPretty a where
  prettyCoal :: a -> Doc ann
  prettyCoal = prettyCoalPrec 0

  prettyCoalPrec :: Int -> a -> Doc ann
  prettyCoalPrec _ = prettyCoal

  {-# MINIMAL prettyCoal | prettyCoalPrec #-}

instance CoalPretty (TypeIndex k) where
  prettyCoalPrec _ (TypeIndex _ i) = pretty (intToVar i)

instance CoalPretty (Parameter k) where
  prettyCoalPrec _ (Parameter _ name) = pretty name

instance CoalPretty Kind where
  prettyCoalPrec = prettyKindPrec

prettyKindPrec :: Prec -> Kind -> Doc ann
prettyKindPrec prec =
  \case
    KType ->
      "*"
    KRow ->
      "Row"
    KTrait ->
      "Trait"
    KVariable v ->
      "k." <> pretty v
    KArrow k1 k2 ->
      parensIf (prec > precArrow) $
        group (prettyKindPrec (precArrow + 1) k1 <+> "→" <+> prettyKindPrec precArrow k2)

instance (Show (o k), Show k, CoalPretty k, CoalPretty (o k)) => CoalPretty (Type o k) where
  prettyCoalPrec = prettyTypePrec

prettyTypePrec :: (Show (o k), Show k, CoalPretty k, CoalPretty (o k)) => Prec -> Type o k -> Doc ann
prettyTypePrec prec =
  \case
    TArrow t1 t2 ->
      parensIf (prec > precArrow) $
        group (prettyTypePrec (precArrow + 1) t1 <+> "->" <+> prettyTypePrec precArrow t2)
    t@TApplication{} ->
      uncurry (prettyTypeApplicationPrec prec) (typeArgs t)
    TConstructor _ name ->
      pretty name
    TVariable v ->
      prettyCoalPrec precAtom v
    TIntrinsic i ->
      prettyIntrinsic i
    TRecord t ->
      braces $ enclose space space (prettyTypePrec precAtom t)
    TRow row ->
      prettyRow (prettyTypePrec precAtom) row
    TAlias name args t ->
      parensIf (prec > precApp) $
        group $
          "type alias"
            <+> (pretty name <> prettyArgs)
            <+> "="
            <+> prettyTypePrec precArrow t
     where
      prettyArgs
        | null args = ""
        | otherwise = typeBrackets (map (prettyTypePrec 0) args)

prettyTypeApplicationPrec :: (Show (o k), Show k, CoalPretty k, CoalPretty (o k)) => Prec -> Type o k -> NonEmpty (Type o k) -> Doc ann
prettyTypeApplicationPrec prec con args
  | isTupleType con =
      parensIf (prec > precApp) $ group (tupled (map (prettyTypePrec 0) (NonEmpty.toList args)))
prettyTypeApplicationPrec prec con args =
  parensIf (prec > precApp) $
    group (prettyTypePrec precApp con <> typeBrackets (map (prettyTypePrec 0) (NonEmpty.toList args)))

prettyIntrinsic :: Intrinsic -> Doc ann
prettyIntrinsic =
  \case
    IBool -> "bool"
    IChar -> "char"
    IDouble -> "double"
    IFloat -> "float"
    IInt32 -> "int32"
    IInt64 -> "int64"
    IBignum -> "bignum"
    INat -> "nat"
    IString -> "string"
    IUnit -> "unit"
    IVoid -> "void"

prettyRow :: (CoalPretty (o k)) => (t -> Doc ann) -> Row o k t -> Doc ann
prettyRow prettyT = fields
 where
  fields =
    \case
      RExtend name ty rest ->
        pretty name <+> ":" <+> prettyT ty <> fieldSep rest
       where
        fieldSep RNil = mempty
        fieldSep RVariable{} = " |" <+> fields rest
        fieldSep _ = "," <+> fields rest
      RVariable v ->
        prettyCoal v
      RNil ->
        "{}"

instance (CoalPretty k, CoalPretty (o k), CoalPretty t) => CoalPretty (Scheme o k t) where
  prettyCoalPrec _ (Forall _ ts t) =
    prettyCoal t <> traits
   where
    traits
      | Set.null ts = ""
      | otherwise = " with " <> hsep (intersperse "," (prettyCoal <$> Set.toList ts))

instance (CoalPretty t) => CoalPretty (Trait t) where
  prettyCoalPrec _ (Trait name t) =
    pretty name <> "<" <> prettyCoal t <> ">"
