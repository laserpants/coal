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
  operatorName,
  parenGroup,
  qualifiedName,
  qualifiedConstructor,
  qualifiedObjectName,
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
    P.notFollowedBy C.alphaNumChar <?> ("end of " <> T.unpack keyword)

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
    else fail $ "Expected exactly " <> show n <> " items, but got " <> show (length items)

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

{- | Parse a field name: a parenthesized operator, an identifier, or a
backtick-quoted string.  Operator fields (e.g. @(+)@, @(==)@) are spelled with
the enclosing parentheses, as produced by the pretty-printer for record rows
and field projections.
-}
field :: Parser Text
field =
  P.choice
    [ P.try operatorName
    , P.try backtickString
    , word
    ]

-- | Characters that may occur in an operator name, e.g. @+@, @==@, @<>@.
operatorChar :: Parser Char
operatorChar = P.satisfy (`elem` ("+-*/%^<>=!&|~?:" :: String))

{- | Parse a parenthesized operator name: @(+)@, @(==)@, @(<>)@.

The result includes the enclosing parentheses, matching how operator-named
functions and record fields are spelled throughout the kernel language.
-}
operatorName :: Parser Text
operatorName =
  lexeme $ do
    void $ C.char '('
    sym <- P.some operatorChar
    void $ C.char ')'
    return $ "(" <> T.pack sym <> ")"

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

{- | Parse a balanced parenthesised group, returning its text verbatim:
@(+)@, @(TIntrinsic(IInt32))@, @(n : int32)@.  Nested parentheses are allowed.
-}
parenGroup :: Parser Text
parenGroup = lexeme pParenGroupRaw

pParenGroupRaw :: Parser Text
pParenGroupRaw = do
  void $ C.char '('
  body <- P.manyTill parenChar (C.char ')')
  return $ "(" <> T.concat body <> ")"

-- | One character of a parenthesised group, recursing into nested groups.
parenChar :: Parser Text
parenChar = pParenGroupRaw <|> (T.singleton <$> P.anySingle)

{- | Parse an object name: a qualified identifier with optional embedded
parenthesised groups, e.g. @Builtin$.(+)@, @Main.(==)@ or
@Builtin$.(+)__$impl_NumericBase(TIntrinsic(IInt32))@.

A group is treated as part of the name only when it is /not/ directly followed
by @=@.  That keeps @Main.from_int32(x : int32) = ...@ parsing as the name
@Main.from_int32@ followed by a parameter list, while a serialised type
argument such as @(TIntrinsic(IInt32))@ is absorbed into the name.
-}
qualifiedObjectName :: Parser Char -> Parser Text
qualifiedObjectName initial = do
  prefix <- P.option "" (T.pack <$> P.some (C.char '$'))
  firstChar <- initial
  rest <- P.many namePart
  spaces
  return $ prefix <> T.singleton firstChar <> T.concat rest
 where
  namePart = (T.singleton <$> validChar) <|> P.try nameSuffixGroup
  nameSuffixGroup = do
    group <- parenGroup
    P.notFollowedBy (P.try (spaces *> C.char '='))
    return group

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
