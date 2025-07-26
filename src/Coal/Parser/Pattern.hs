{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Pattern (parsePattern) where

import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.Parser
import Coal.Parser.Identifier
import Coal.Parser.Symbol
import Coal.Ast.Metadata (Metadata (..))
import Coal.Parser.Type (parseType)
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Text.Megaparsec (option, optional, (<|>))
import Text.Megaparsec.Char (char)

parsePattern :: Parser (Pattern Metadata ())
parsePattern = makeExprParser go operator
 where
  go = do
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
    pure (maybe p1 (\n -> PAs undefined n p1) rest)

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
parseAnyPattern = symbol "_" $> PAny undefined ()

parseVariablePattern :: Parser (Pattern Metadata ())
parseVariablePattern = PVariable undefined . Label () <$> name

parseLiteralPattern :: Parser (Pattern Metadata ())
parseLiteralPattern = parseListLiteralPattern

parseListLiteralPattern :: Parser (Pattern Metadata ())
parseListLiteralPattern = PListLiteral undefined () <$> brackets (commaSep parsePattern)

parseAtVariablePattern :: Parser (Pattern Metadata ())
parseAtVariablePattern = do
  void (char '@')
  PAtVariable undefined . Label () <$> name

parseConstructorPattern :: Parser (Pattern Metadata ())
parseConstructorPattern =
  PConstructor undefined . Label ()
    <$> constructor
    <*> option [] (parens (commaSep1 parsePattern))
