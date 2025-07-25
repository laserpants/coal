{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Pattern (parsePattern) where

import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.Parser
import Coal.Parser.Identifier
import Coal.Parser.Symbol
import Coal.Parser.Type (parseType)
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Text.Megaparsec (option, optional, (<|>))
import Text.Megaparsec.Char (char)

parsePattern :: Parser (Pattern () ())
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
    pure (maybe p1 (\n -> PAs () n p1) rest)

operator :: [[Operator Parser (Pattern () ())]]
operator =
  [ -- TODO

    [ InfixR (PListCons () () <$ symbol "::")
    ]
  , -- TODO

    [ InfixL (POr () () <$ lexeme "or")
    ]
  , [Postfix (symbol_ ":" *> (PAnnotation () <$> parseType))]
  ]

parseAnyPattern :: Parser (Pattern () ())
parseAnyPattern = symbol "_" $> PAny () ()

parseVariablePattern :: Parser (Pattern () ())
parseVariablePattern = PVariable () . Label () <$> name

parseLiteralPattern :: Parser (Pattern () ())
parseLiteralPattern = parseListLiteralPattern

parseListLiteralPattern :: Parser (Pattern () ())
parseListLiteralPattern = PListLiteral () () <$> brackets (commaSep parsePattern)

parseAtVariablePattern :: Parser (Pattern () ())
parseAtVariablePattern = do
  void (char '@')
  PAtVariable () . Label () <$> name

parseConstructorPattern :: Parser (Pattern () ())
parseConstructorPattern =
  PConstructor () . Label ()
    <$> constructor
    <*> option [] (parens (commaSep1 parsePattern))
