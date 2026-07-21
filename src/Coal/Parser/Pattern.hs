{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Coal.Parser.Pattern

Parsers for pattern matching constructs.

Handles all pattern forms including variables, constructors, literals,
records, lists, tuples, wildcards, or-patterns, and as-patterns.
-}
module Coal.Parser.Pattern (parsePattern, parseUnitPattern) where

import Coal.Common.Label (Label (..))
import Coal.Compiler.HasMetadata (metadataSpan)
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Language (Pattern (..), Primitive (LUnit))
import Coal.Parser.Common (parseQualifiedConstructor, parseSimpleConstructor)
import Coal.Parser.Core (Parser, lexeme, lexeme_, spaces)
import Coal.Parser.Identifier (name, validChar)
import Coal.Parser.Metadata (withMetadata)
import qualified Coal.Parser.Primitive as Primitive
import Coal.Parser.Symbol
import Coal.Parser.Type (parseType)
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Extras (Name)
import Text.Megaparsec (notFollowedBy, option, optional, some, try, (<|>))
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as Lexer

parseAtom :: Parser (Pattern Metadata () ())
parseAtom =
  parseConstructorPattern
    <|> parseAtVariablePattern
    <|> parseLiteralPattern
    <|> parseRecordPattern
    <|> try parseWildcardPattern
    <|> try parseAtFunction
    <|> parseVariablePattern
    <|> try parseUnitLiteral
    <|> try (parens parsePattern)
    <|> parseTuplePattern

parsePattern :: Parser (Pattern Metadata () ())
parsePattern = makeExprParser parseAtom patternOperators

parseUnitPattern :: Parser (NonEmpty (Pattern Metadata () ()))
parseUnitPattern = withMetadata $ pure (\loc -> PLiteral loc LUnit :| [])

patternOperator :: (Metadata -> () -> Pattern Metadata () () -> Pattern Metadata () () -> Pattern Metadata () ()) -> Pattern Metadata () () -> Pattern Metadata () () -> Pattern Metadata () ()
patternOperator op p1 p2 = op (metadataSpan p1 p2) () p1 p2

annotation :: Parser (Pattern Metadata () () -> Pattern Metadata () ())
annotation = do
  withMetadata $ do
    symbol_ ":"
    t <- parseType
    pure (`PAnnotation` t)

asPattern :: Parser (Pattern Metadata () () -> Pattern Metadata () ())
asPattern = do
  withMetadata $ do
    lexeme_ "as"
    p2 <- parseVariablePattern
    case p2 of
      PVariable _ (Label _ n) ->
        pure (\loc -> PAs loc (Label () n))
      _ ->
        fail "Expected a variable on the right-hand side of 'as'"

patternOperators :: [[Operator Parser (Pattern Metadata () ())]]
patternOperators =
  [ [InfixR (patternOperator PListCons <$ symbol_ "::")]
  , [InfixL (patternOperator POr <$ lexeme "or")]
  , [Postfix (foldl (.) id <$> some (asPattern <|> annotation))]
  ]

parseWildcardPattern :: Parser (Pattern Metadata () ())
parseWildcardPattern =
  withMetadata $ do
    lexeme_ $ do
      void (char '_')
      notFollowedBy validChar
    pure (`PAny` ())

parseVariablePattern :: Parser (Pattern Metadata () ())
parseVariablePattern =
  withMetadata $ do
    ll <- Label () <$> name
    pure (`PVariable` ll)

parseLiteralPattern :: Parser (Pattern Metadata () ())
parseLiteralPattern =
  parseListLiteralPattern
    <|> parseIntegerLiteral
    <|> parseBasicLiteralPattern

parseIntegerLiteral :: Parser (Pattern Metadata () ())
parseIntegerLiteral =
  withMetadata $ do
    n <- Lexer.signed spaces (lexeme Lexer.decimal)
    pure (\loc -> PInteger loc () n)

parseBasicLiteralPattern :: Parser (Pattern Metadata () ())
parseBasicLiteralPattern = do
  withMetadata $ do
    lit <- Primitive.parseAtom
    pure (`PLiteral` lit)

parseListLiteralPattern :: Parser (Pattern Metadata () ())
parseListLiteralPattern =
  withMetadata $ do
    ps <- brackets (commaSep parsePattern)
    pure (\loc -> PListLiteral loc () ps)

parseAtVariablePattern :: Parser (Pattern Metadata () ())
parseAtVariablePattern = do
  withMetadata $ do
    p <- parseAtVar
    pure (`PAtVariable` p)

parseAtFunction :: Parser (Pattern Metadata () ())
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

parseConstructorPattern :: Parser (Pattern Metadata () ())
parseConstructorPattern =
  withMetadata $ do
    ll <- try parseQualifiedConstructor <|> parseSimpleConstructor
    ps <- option [] (parens (commaSep1 parsePattern))
    pure (\loc -> PConstructor loc ll ps)

recordFields :: Parser [(Name, Pattern Metadata () ())]
recordFields = commaSep1 (try normalField <|> shorthand)
 where
  normalField = do
    n <- name
    symbol_ "="
    p <- parsePattern
    pure (n, p)
  shorthand = do
    withMetadata $ do
      n <- name
      pure (\loc -> (n, PShorthand loc (Label () n)))

parseRecordPattern :: Parser (Pattern Metadata () ())
parseRecordPattern =
  withMetadata $ do
    braces $ do
      fields <- recordFields
      tail_ <- optional rest
      pure (\loc -> PRecord loc () (Map.fromList fields) tail_)
 where
  rest = pipe >> parsePattern

parseUnitLiteral :: Parser (Pattern Metadata () ())
parseUnitLiteral =
  withMetadata $ do
    _ <- symbol "("
    _ <- symbol ")"
    pure (`PLiteral` LUnit)

parseTuplePattern :: Parser (Pattern Metadata () ())
parseTuplePattern = do
  withMetadata $ do
    parens $ do
      pats <- commaSep1 parsePattern
      case pats of
        [] -> fail "Empty tuple"
        (p : ps) -> pure (\loc -> PTuple loc () (p :| ps))
