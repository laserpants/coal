{-# LANGUAGE OverloadedStrings #-}

{- |
Type expression pretty printing.

Renders type expressions with proper precedence and parenthesization:

  * Function types: @A/B@ (right-associative)
  * Type constructors: @list[int32]@
  * Record types: @{x:int32, y:int32}@
  * Row extension: @{x:int32 | row}@
  * Opaque wildcard: @*@

= Precedence handling

Function types require parentheses when appearing as arguments to other
function types. The pretty printer tracks precedence levels to insert
parentheses only where necessary.
-}
module Coal.Kernel.Prettyprinter.Type (
  prettyType,
) where

import qualified Data.Text as T

import Prettyprinter (Doc, Pretty (..), parens, (<+>))

import Coal.Kernel.Language.Type (Type (..))

-- | Pretty print a type expression
prettyType :: Type -> Doc ann
prettyType = prettyTypePrec 0

{- | Pretty print a type with precedence for proper parenthesization.

= Precedence levels

  * 0: top level (no parens needed)
  * 1: function type right-hand side
  * 2: function type argument (needs parens if it's also a function)
-}
prettyTypePrec :: Int -> Type -> Doc ann
prettyTypePrec prec (TCon "/" [t1, t2]) =
  -- Function type: right-associative, use /
  -- Add parens when this function type appears as an argument to another function
  let inner = prettyTypePrec 2 t1 <> "/" <> prettyTypePrec 1 t2
   in if prec >= 2 then parens inner else inner
prettyTypePrec _ (TCon name args)
  | name == "list"
  , [arg] <- args =
      "list" <> parens (prettyType arg)
  | T.isPrefixOf "tuple" name && not (null args) =
      -- tuple2, tuple3, etc. - NO SPACES
      pretty name <> parens (prettyCommaSep args)
  | name == "record"
  , [arg] <- args =
      -- record types: the argument is already a row (with braces)
      "record" <> parens (prettyType arg)
  | null args =
      pretty name
  | otherwise =
      pretty name <> parens (prettyCommaSep args)
prettyTypePrec _ TOpq = "*"
prettyTypePrec _ (RExt field typ rest) =
  -- Row extensions are always wrapped in braces with spaces inside
  -- The rest part is printed without additional braces using prettyRowRest
  "{ " <> pretty field <+> ":" <+> prettyType typ <+> "|" <+> prettyRowRest rest <> " }"
prettyTypePrec _ RNil = "{}"

-- | Pretty print the rest/tail of a row (without outer braces)
prettyRowRest :: Type -> Doc ann
prettyRowRest RNil = "{}"
prettyRowRest TOpq = "*"
prettyRowRest (RExt field typ rest) =
  -- Nested row extensions are printed without outer braces
  pretty field <+> ":" <+> prettyType typ <+> "|" <+> prettyRowRest rest
prettyRowRest t = prettyType t -- fallback for other types

-- | Pretty print a comma-separated list of types
prettyCommaSep :: [Type] -> Doc ann
prettyCommaSep [] = mempty
prettyCommaSep [t] = prettyType t
prettyCommaSep (t : ts) = prettyType t <> "," <+> prettyCommaSep ts
