{-# LANGUAGE OverloadedStrings #-}

{- |
Primitive value parser.

Parses primitive literals:

  * Unit (@()@)
  * Booleans (@true@, @false@)
  * Integers (@int32@, @int64@, @bignum@)
  * Floating-point numbers (@float@, @double@)
  * Characters (Unicode code points)
  * Strings (UTF-8 encoded, backtick-delimited)

All numeric literals use standard decimal notation with optional type suffixes.
-}
module Coal.Kernel.Parser.Prim (
  prim,
) where

import Control.Monad (void)
import Data.Char (ord)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Kernel.Parser (Parser, lexeme, reserved)

-- | Parse a primitive value
prim :: Parser Prim
prim =
  P.choice
    [ P.try pBignum
    , P.try pInt64
    , P.try pFloat
    , P.try pDouble
    , P.try pInt32
    , pUnit
    , pBool
    , pString
    , pChar
    ]

-- | Parse unit value: ()
pUnit :: Parser Prim
pUnit = lexeme $ do
  void $ C.string "()"
  return PUnit

-- | Parse boolean: true or false
pBool :: Parser Prim
pBool =
  (reserved "true" >> return (PBool True))
    <|> (reserved "false" >> return (PBool False))

-- | Parse a character literal: 'x' (with escape sequences)
pChar :: Parser Prim
pChar = lexeme $ do
  void $ C.char '\''
  c <- charLiteral
  void $ C.char '\''
  return $ PChar (fromIntegral $ ord c)

-- | Parse a string literal: "..." (with escape sequences)
pString :: Parser Prim
pString = lexeme $ do
  void $ C.char '"'
  str <- P.many stringChar
  void $ C.char '"'
  return $ PString (TE.encodeUtf8 $ T.pack str)

-- | Parse a float: d.d+ followed by 'f' or 'F'
pFloat :: Parser Prim
pFloat = lexeme $ do
  sign <- L.signed (return ()) (L.float :: Parser Double)
  _ <- C.char 'f' <|> C.char 'F'
  return $ PFloat (realToFrac sign :: Float)

-- | Parse a double: d.d+ (no suffix)
pDouble :: Parser Prim
pDouble = lexeme $ do
  sign <- L.signed (return ()) L.float
  P.notFollowedBy (C.char 'f' <|> C.char 'F')
  return $ PDouble sign

-- | Parse a bignum: %% followed by integer
pBignum :: Parser Prim
pBignum = lexeme $ do
  void $ C.string "%%"
  num <- L.signed (return ()) L.decimal
  return $ PBignum num

-- | Parse an int64: % followed by integer (but not %%)
pInt64 :: Parser Prim
pInt64 = lexeme $ do
  void $ C.char '%'
  P.notFollowedBy (C.char '%')
  num <- L.signed (return ()) L.decimal
  return $ PInt64 num

-- | Parse an int32: plain signed integer (no prefix, no decimal)
pInt32 :: Parser Prim
pInt32 = lexeme $ do
  P.notFollowedBy (C.char '%')
  num <- L.signed (return ()) L.decimal
  P.notFollowedBy (C.char '.')
  return $ PInt32 num

-- Helper: Parse a character inside a char literal (handles escape sequences)
charLiteral :: Parser Char
charLiteral = escapeSequence <|> P.satisfy (/= '\'')

-- Helper: Parse a character inside a string literal (handles escape sequences)
stringChar :: Parser Char
stringChar = escapeSequence <|> P.satisfy (/= '"')

-- Helper: Parse escape sequences
escapeSequence :: Parser Char
escapeSequence = do
  void $ C.char '\\'
  P.choice
    [ C.char 'n' >> return '\n'
    , C.char 't' >> return '\t'
    , C.char 'r' >> return '\r'
    , C.char 'b' >> return '\b'
    , C.char 'f' >> return '\f'
    , C.char '\\' >> return '\\'
    , C.char '\'' >> return '\''
    , C.char '"' >> return '"'
    , C.char '0' >> return '\0'
    ]
