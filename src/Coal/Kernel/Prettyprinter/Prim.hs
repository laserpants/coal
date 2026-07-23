{-# LANGUAGE OverloadedStrings #-}

{- |
Primitive value pretty printing.

Renders primitive literals with type-specific formatting:

  * Unit: @()@
  * Booleans: @true@, @false@
  * Integers: @42@ (int32), @%42@ (int64), @%%42@ (bignum)
  * Floats: @3.14f@ (float), @3.14@ (double)
  * Characters: Unicode escapes or direct characters
  * Strings: Backtick-delimited with Unicode escapes
-}
module Coal.Kernel.Prettyprinter.Prim (
  prettyPrim,
) where

import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Prettyprinter (Doc, Pretty (..))

import Coal.Kernel.Language.Prim (Prim (..))

-- | Pretty print a primitive value
prettyPrim :: Prim -> Doc ann
prettyPrim PUnit = "()"
prettyPrim (PBool True) = "true"
prettyPrim (PBool False) = "false"
prettyPrim (PInt32 n) = pretty n
prettyPrim (PInt64 n) = "%" <> pretty n
prettyPrim (PBignum n) = "%%" <> pretty n
prettyPrim (PFloat f) = pretty f <> "f"
prettyPrim (PDouble d) = pretty d
prettyPrim (PChar c) = prettyChar c
prettyPrim (PString bs) = prettyString bs

-- | Pretty print a character with proper escaping
prettyChar :: Int32 -> Doc ann
prettyChar c = "'" <> escapeChar (fromIntegral c) <> "'"
 where
  escapeChar :: Int -> Doc ann
  escapeChar ch =
    case ch of
      10 ->
        "\\n" -- newline
      9 ->
        "\\t" -- tab
      0 ->
        "\\0" -- null
      92 ->
        "\\\\" -- backslash
      39 ->
        "\\'" -- single quote
      34 ->
        "\\\"" -- double quote (for consistency)
      _
        | ch >= 32 && ch <= 126 ->
            pretty (toEnum ch :: Char)
      _
        | ch > 126 ->
            pretty (toEnum ch :: Char) -- Unicode characters
      _ ->
        "\\" <> pretty ch -- control characters (0-31, 127)

-- | Pretty print a string with proper escaping
prettyString :: ByteString -> Doc ann
prettyString bs = "\"" <> escapeString text <> "\""
 where
  text = TE.decodeUtf8 bs

  escapeString :: Text -> Doc ann
  escapeString = T.foldl' (\acc ch -> acc <> escapeStringChar ch) mempty

  escapeStringChar :: Char -> Doc ann
  escapeStringChar ch = case ch of
    '\n' ->
      "\\n"
    '\t' ->
      "\\t"
    '\0' ->
      "\\0"
    '\\' ->
      "\\\\"
    '"' ->
      "\\\""
    _
      | ord ch >= 32 && ord ch <= 126 ->
          pretty ch
    _
      | ord ch > 126 ->
          pretty ch -- Unicode characters
    _ ->
      "\\" <> pretty (ord ch) -- control characters
