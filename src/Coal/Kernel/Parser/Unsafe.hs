{-# LANGUAGE OverloadedStrings #-}

{- |
Runtime parsing helper for kernel expressions.

Provides 'unsafeParseExpr', which parses a kernel-language expression from a
'String' at runtime, throwing an 'error' if parsing fails.  Intended for use
in hand-written builtin object definitions where the bodies are written as
string literals (typically with the @raw-strings-qq@ quasi-quoter) and parsed
on startup.
-}
module Coal.Kernel.Parser.Unsafe (unsafeParseExpr) where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec (errorBundlePretty, runParser)

import Coal.Kernel.Language.Expr (Expr)
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Parser.Expr (expr)

{- | Parse a kernel expression from a raw string.  Calls 'error' with a
formatted message if parsing fails.  Only use this for statically-known strings
that are guaranteed to be valid kernel expressions (e.g. builtin bodies).
-}
unsafeParseExpr :: String -> Expr Type
unsafeParseExpr src =
  case runParser expr "<builtin>" (Text.pack src) of
    Left err -> error ("unsafeParseExpr: " <> errorBundlePretty err)
    Right e  -> e
