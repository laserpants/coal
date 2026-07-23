{-# LANGUAGE OverloadedStrings #-}

{- |
Shared parser combinators and utilities.

Provides the base 'Parser' type and common combinators for parsing Coal kernel
language source files. Implements lexing with support for:

  * Whitespace handling (including line and block comments)
  * Reserved keywords
  * Qualified identifiers
  * Bracketing and delimiters

All parsers consume trailing whitespace automatically via the 'lexeme'
combinator.
-}
module Coal.Kernel.Parser (
  Parser,
  spaces,
  lexeme,
  reserved,
  word,
  brackets,
  parens,
  pair,
  commaSep,
  commaSepN,
  backtickString,
  constructor,
  field,
  qualifiedName,
  qualifiedConstructor,
) where

import Control.Monad (void)
import Data.Char (isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)

import Text.Megaparsec (Parsec, (<?>), (<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parser type alias
type Parser = Parsec Void Text

spaces :: Parser ()
spaces =
  L.space
    C.space1
    (L.skipLineComment "//")
    (L.skipBlockComment "{-" "-}")

-- | Parse something and consume trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

-- | Parse a reserved keyword (exact match, not followed by alphanumeric)
reserved :: Text -> Parser ()
reserved keyword =
  lexeme $ P.try $ do
    void $ C.string keyword
    P.notFollowedBy C.alphaNumChar <?> ("end of " ++ T.unpack keyword)

-- | Parse a word (identifier): starts with letter or underscore, followed by alphanumeric or underscore
word :: Parser Text
word =
  lexeme $ do
    first <- C.letterChar <|> C.char '_' <|> C.char '$'
    rest <- P.many (C.alphaNumChar <|> C.char '_')
    return $ T.pack (first : rest)

-- | Parse brackets: [ ... ]
brackets :: Parser a -> Parser a
brackets p = do
  void $ lexeme (C.char '[')
  result <- p
  void $ lexeme (C.char ']')
  return result

-- | Parse parentheses: ( ... )
parens :: Parser a -> Parser a
parens p = do
  void $ lexeme (C.char '(')
  result <- p
  void $ lexeme (C.char ')')
  return result

-- | Parse a pair: expr1, expr2
pair :: Parser a -> Parser b -> Parser (a, b)
pair p1 p2 = do
  x <- p1
  void $ lexeme (C.char ',')
  y <- p2
  return (x, y)

-- | Parse a comma-separated list of items
commaSep :: Parser a -> Parser [a]
commaSep p = P.sepBy p (lexeme (C.char ','))

-- | Parse exactly N comma-separated items
commaSepN :: Int -> Parser a -> Parser [a]
commaSepN n p = do
  items <- P.sepBy1 p (lexeme (C.char ','))
  if length items == n
    then return items
    else fail $ "Expected exactly " ++ show n ++ " items, but got " ++ show (length items)

-- | Parse a backtick-quoted string: `string`
backtickString :: Parser Text
backtickString =
  lexeme $ do
    void $ C.char '`'
    str <- P.many (P.satisfy (/= '`'))
    void $ C.char '`'
    return $ T.pack str

-- | Parse a type constructor name (starts with uppercase)
constructor :: Parser Text
constructor =
  lexeme $ do
    first <- C.upperChar
    rest <- P.many (C.alphaNumChar <|> C.char '_')
    return $ T.pack (first : rest)

-- | Parse a field name (identifier or backtick-quoted string)
field :: Parser Text
field =
  P.choice
    [ P.try backtickString
    , word
    ]

{-# INLINE validChar #-}
validChar :: Parser Char
validChar = C.alphaNumChar <|> C.char '.' <|> C.char '$' <|> C.char '_'

{- | Parse a qualified name (dotted identifier) with optional $ prefix
Examples: My.Utilities.find_min, $Cons, Main.sample_data
-}
qualifiedName :: Parser Char -> Parser Text
qualifiedName initial =
  lexeme $ do
    -- Optional $ prefix (can be multiple $'s)
    prefix <- P.option "" (T.pack <$> P.some (C.char '$'))
    -- Parse first segment
    firstChar <- initial
    restChars <- P.many validChar
    let firstSegment = T.pack (firstChar : restChars)
    -- Parse optional dotted segments
    segments <- P.many $ do
      void $ C.char '.'
      segFirst <- C.letterChar <|> C.char '_'
      segRest <- P.many (C.alphaNumChar <|> C.char '_')
      return $ T.pack (segFirst : segRest)
    -- Combine all parts
    return $ prefix <> T.intercalate "." (firstSegment : segments)

{- | Parse a qualified constructor: last dot-separated component must start with uppercase.
Examples: Maybe, $Cons, My.Data.Maybe, Main.Node
Counter-examples: Main.sort, My.Utilities.find_min
-}
qualifiedConstructor :: Parser Text
qualifiedConstructor =
  P.try $ do
    name <- qualifiedName C.upperChar
    let lastComp = T.dropWhile (== '$') $ last $ T.splitOn "." name
    if not (T.null lastComp) && isUpper (T.head lastComp)
      then return name
      else P.empty
