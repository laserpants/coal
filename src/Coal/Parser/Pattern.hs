{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Pattern (parsePattern, parseUnitPattern) where

import Coal.Ast.Metadata (Metadata (..), getMetadata, metadataSpan)
import Coal.Common.Label (Label (..))
import Coal.Common.List1 (List1 (..), NonEmpty (..))
import Coal.Language
import Coal.Parser
import Coal.Parser.Identifier
import Coal.Parser.Metadata
import Coal.Parser.Symbol
import Coal.Parser.Type (parseType)
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Text.Megaparsec (getSourcePos, option, optional, (<|>))
import Text.Megaparsec.Char (char)

parseUnitPattern :: Parser (List1 (Pattern Metadata ()))
parseUnitPattern =
  withMetadata $ do
    pure (\loc -> PLiteral loc LUnit :| [])

parsePattern :: Parser (Pattern Metadata ())
parsePattern = makeExprParser go operator
 where
  go = do
    start <- getSourcePos
    p1 <-
      parseConstructorPattern
        <|> parseAtVariablePattern
        <|> parseLiteralPattern
        <|> parseAnyPattern
        <|> parseVariablePattern
        <|> parens parsePattern
    rest <- optional $ do
      lexeme_ "as"
      p2 <- parsePattern
      case p2 of
        PVariable _ (Label _ n) ->
          pure (Label () n)
        _ ->
          fail "Expected variable on right-hand side of 'as'"
    end <- getSourcePos
    pure (maybe p1 (\n -> PAs (Metadata start end) n p1) rest)

patternOperator :: (Metadata -> () -> Pattern Metadata () -> Pattern Metadata () -> Pattern Metadata ()) -> Pattern Metadata () -> Pattern Metadata () -> Pattern Metadata ()
patternOperator op p1 p2 = op (metadataSpan p1 p2) () p1 p2

annotation :: Parser (Pattern Metadata () -> Pattern Metadata ())
annotation = do
  start <- getSourcePos
  symbol_ ":"
  t <- parseType
  end <- getSourcePos
  pure (PAnnotation (Metadata start end) t)

operator :: [[Operator Parser (Pattern Metadata ())]]
operator =
  [ -- TODO

    [ InfixR (patternOperator PListCons <$ symbol_ "::")
    ]
  , -- TODO

    [ InfixL (patternOperator POr <$ lexeme "or")
    ]
  , [Postfix annotation]
  ]

parseAnyPattern :: Parser (Pattern Metadata ())
parseAnyPattern =
  withMetadata $ do
    symbol "_"
    pure (`PAny` ())

parseVariablePattern :: Parser (Pattern Metadata ())
parseVariablePattern =
  withMetadata $ do
    ll <- Label () <$> name
    pure (`PVariable` ll)

parseLiteralPattern :: Parser (Pattern Metadata ())
parseLiteralPattern = parseListLiteralPattern

parseListLiteralPattern :: Parser (Pattern Metadata ())
parseListLiteralPattern =
  withMetadata $ do
    ps <- brackets (commaSep parsePattern)
    pure (\loc -> PListLiteral loc () ps)

parseAtVariablePattern :: Parser (Pattern Metadata ())
parseAtVariablePattern = do
  withMetadata $ do
    void (char '@')
    ll <- Label () <$> name
    pure (`PAtVariable` ll)

parseConstructorPattern :: Parser (Pattern Metadata ())
parseConstructorPattern =
  withMetadata $ do
    ll <- Label () <$> constructor
    ps <- option [] (parens (commaSep1 parsePattern))
    pure (\loc -> PConstructor loc ll ps)
