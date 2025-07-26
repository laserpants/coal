{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Pattern (parsePattern, parseUnitPattern) where

import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.Parser
import Coal.Parser.Identifier
import Coal.Common.List1 (NonEmpty (..), List1 (..))
import Coal.Parser.Symbol
import Coal.Ast.Metadata (Metadata (..))
import Coal.Parser.Metadata
import Coal.Parser.Type (parseType)
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Text.Megaparsec (option, optional, (<|>), getSourcePos)
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

operator :: [[Operator Parser (Pattern Metadata ())]]
operator =
  [ -- TODO

    [ InfixR (PListCons undefined () <$ symbol "::")
    ]
  , -- TODO

    [ InfixL (POr undefined () <$ lexeme "or")
    ]
  , [Postfix (symbol_ ":" *> (PAnnotation undefined <$> parseType))]
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
