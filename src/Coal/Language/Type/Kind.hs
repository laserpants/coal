{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Type.Kind (
  Kind (..),
  foldKind,
  unfoldKind,
  applyKind,
) where

import Coal.Common.List1 (List1, fromList1, (<|))
import Coal.Pretty.Utils (parensIf)
import Data.Data (Data, Typeable)
import Data.List (isPrefixOf)
import GHC.Generics (Generic)
import Prettyprinter

import qualified Coal.Common.List1 as List1

data Kind
  = KType
  | KRow
  | KArrow Kind Kind
  | KTrait
  deriving (Show, Eq, Ord, Read, Data, Typeable, Generic)

infixr 1 `KArrow`

{-# INLINE foldKind #-}
foldKind :: (Foldable f) => Kind -> f Kind -> Kind
foldKind = foldr KArrow

unfoldKind :: Kind -> List1 Kind
unfoldKind =
  \case
    KArrow k1 k2 ->
      k1 <| unfoldKind k2
    k ->
      List1.singleton k

applyKind :: [Kind] -> Kind -> Maybe Kind
applyKind ks k
  | ks `isPrefixOf` ls && length ls > length ks =
      Just (foldr1 KArrow (drop (length ks) ls))
  | otherwise =
      Nothing
 where
  ls = fromList1 (unfoldKind k)

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
