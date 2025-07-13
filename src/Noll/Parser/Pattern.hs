{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Pattern (patternParser) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Lang.Label (Label (..))
import Noll.Language
import Noll.Parser
import Noll.Parser.Identifier
import Noll.Parser.Symbol
import Noll.Parser.Type
import Text.Megaparsec (option, optional, (<|>))
import Text.Megaparsec.Char (char)

patternParser :: Parser (Pattern () ())
patternParser = makeExprParser go operator
 where
  go = do
    p1 <-
      constructorPattern
        <|> atVariablePattern
        <|> variablePattern
    rest <- optional $ do
      _ <- lexeme "as"
      p2 <- patternParser
      case p2 of
        PVariable _ (Label _ n) ->
          return (Label () n)
        _ ->
          fail "Expected variable on right-hand side of 'as'"
    return $
      case rest of
        Just n ->
          PAs () n p1
        Nothing ->
          p1

operator :: [[Operator Parser (Pattern () ())]]
operator =
  [ -- TODO
    [Postfix typeAnnotation]
  ]

typeAnnotation :: Parser (Pattern () () -> Pattern () ())
typeAnnotation = do
  void (symbol ":")
  ty <- typeParser
  pure (PAnnotation () ty)

variablePattern :: Parser (Pattern () ())
variablePattern = PVariable () . Label () <$> name

literalPattern :: Parser (Pattern () ())
literalPattern = undefined

atVariablePattern :: Parser (Pattern () ())
atVariablePattern = do
  void (char '@')
  PAtVariable () . Label () <$> name

constructorPattern :: Parser (Pattern () ())
constructorPattern = do
  c <- constructor
  ps <- option [] (parens (commaSep1 patternParser))
  pure (PConstructor () (Label () c) ps)
