{-# LANGUAGE OverloadedStrings #-}

-- FIXME
module Coal.Parser.Pattern (parsePattern, parseUnitPattern) where

import Coal.Ast.Metadata (Metadata (..), metadataSpan)
import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.Parser
import Coal.Parser.Identifier
import Coal.Parser.Metadata
import Coal.Parser.Symbol
import Coal.Parser.Type (parseType)
import Coal.Parser.Utils (fieldList)
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Char (ord)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Text.Megaparsec (getSourcePos, option, optional, some, try, (<|>))
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as Lexer

parseAtom :: Parser (Pattern Metadata ())
parseAtom =
  parseConstructorPattern
    <|> parseAtVariablePattern
    <|> parseLiteralPattern
    <|> parseRecordPattern
    <|> parseAnyPattern
    <|> try parseAtFunction
    <|> parseVariablePattern
    <|> try (parens parsePattern)
    <|> parseTuplePattern

parsePattern :: Parser (Pattern Metadata ())
parsePattern = makeExprParser parseAtom patternOperators

parseUnitPattern :: Parser (NonEmpty (Pattern Metadata ()))
parseUnitPattern = withMetadata $ pure (\loc -> PLiteral loc LUnit :| [])

patternOperator :: (Metadata -> () -> Pattern Metadata () -> Pattern Metadata () -> Pattern Metadata ()) -> Pattern Metadata () -> Pattern Metadata () -> Pattern Metadata ()
patternOperator op p1 p2 = op (metadataSpan p1 p2) () p1 p2

annotation :: Parser (Pattern Metadata () -> Pattern Metadata ())
annotation = do
  start <- getSourcePos
  symbol_ ":"
  t <- parseType
  end <- getSourcePos
  pure (PAnnotation (Metadata start end) t)

asPattern :: Parser (Pattern Metadata () -> Pattern Metadata ())
asPattern = do
  start <- getSourcePos
  lexeme_ "as"
  p2 <- parseVariablePattern
  end <- getSourcePos
  case p2 of
    PVariable _ (Label _ n) ->
      pure (PAs (Metadata start end) (Label () n))
    _ ->
      fail "Expected a variable on the right-hand side of 'as'"

patternOperators :: [[Operator Parser (Pattern Metadata ())]]
patternOperators =
  [ [InfixR (patternOperator PListCons <$ symbol_ "::")]
  , [InfixL (patternOperator POr <$ lexeme "or")]
  , [Postfix (foldl (.) id <$> some (asPattern <|> annotation))]
  ]

parseAnyPattern :: Parser (Pattern Metadata ())
parseAnyPattern =
  withMetadata $ do
    symbol_ "_"
    pure (`PAny` ())

parseVariablePattern :: Parser (Pattern Metadata ())
parseVariablePattern =
  withMetadata $ do
    ll <- Label () <$> name
    pure (`PVariable` ll)

parseLiteralPattern :: Parser (Pattern Metadata ())
parseLiteralPattern =
  parseListLiteralPattern
    <|> parseLiteralTrue
    <|> parseLiteralFalse
    <|> parseCharLiteralPattern

squote :: Parser Char
squote = char '\''

parseLiteralTrue :: Parser (Pattern Metadata ())
parseLiteralTrue =
  withMetadata $ do
    lexeme_ "true"
    pure (\loc -> PLiteral loc (LBool True))

parseLiteralFalse :: Parser (Pattern Metadata ())
parseLiteralFalse =
  withMetadata $ do
    lexeme_ "false"
    pure (\loc -> PLiteral loc (LBool False))

parseCharLiteralPattern :: Parser (Pattern Metadata ())
parseCharLiteralPattern =
  withMetadata $ do
    lexeme $ do
      void squote
      ch <- Lexer.charLiteral
      void squote
      pure (\loc -> PLiteral loc (LChar (fromIntegral (ord ch))))

parseListLiteralPattern :: Parser (Pattern Metadata ())
parseListLiteralPattern =
  withMetadata $ do
    ps <- brackets (commaSep parsePattern)
    pure (\loc -> PListLiteral loc () ps)

parseAtVariablePattern :: Parser (Pattern Metadata ())
parseAtVariablePattern = do
  withMetadata $ do
    p <- parseAtVar
    pure (`PAtVariable` p)

parseAtFunction :: Parser (Pattern Metadata ())
parseAtFunction = do
  withMetadata $ do
    n <- name
    ll <- parens $ do
      void (char '@')
      Label () <$> name
    pure (\loc -> PNamedFold loc n ll)

parseAtVar :: Parser (Label ())
parseAtVar = do
  void (char '@')
  Label () <$> name

parseConstructorPattern :: Parser (Pattern Metadata ())
parseConstructorPattern =
  withMetadata $ do
    ll <- Label () <$> constructor
    ps <- option [] (parens (commaSep1 parsePattern))
    pure (\loc -> PConstructor loc ll ps)

parseRecordPattern :: Parser (Pattern Metadata ())
parseRecordPattern =
  withMetadata $ do
    braces $ do
      fields <- fieldList parsePattern "="
      tail_ <- optional rest
      pure (\loc -> PRecord loc () (Map.fromList fields) tail_)
 where
  rest = pipe >> parsePattern

parseTuplePattern :: Parser (Pattern Metadata ())
parseTuplePattern = do
  withMetadata $ do
    parens $ do
      pats <- commaSep1 parsePattern
      case pats of
        [] -> fail "Empty tuple"
        (p : ps) -> pure (\loc -> PTuple loc () (p :| ps))
