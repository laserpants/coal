{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Pattern (parsePattern) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Parser
import Noll.Parser.Identifier
import Noll.Parser.Symbol
import Noll.Parser.Type (parseType)
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
    pure $
      case rest of
        Just n ->
          PAs () n p1
        Nothing ->
          p1

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
parseListLiteralPattern = do
  ps <- brackets (commaSep parsePattern)
  pure (PListLiteral () () ps)

parseAtVariablePattern :: Parser (Pattern () ())
parseAtVariablePattern = do
  void (char '@')
  PAtVariable () . Label () <$> name

parseConstructorPattern :: Parser (Pattern () ())
parseConstructorPattern = do
  c <- constructor
  ps <- option [] (parens (commaSep1 parsePattern))
  pure (PConstructor () (Label () c) ps)
