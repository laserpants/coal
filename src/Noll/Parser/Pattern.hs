{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Pattern where

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
        PVariable _ (Label _ name) ->
          return (Label () name)
        _ ->
          fail "Expected variable on right-hand side of 'as'"
    return $
      case rest of
        Just name ->
          PAs () name p1
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
variablePattern = do
  var <- name
  pure (PVariable () (Label () var))

literalPattern :: Parser (Pattern () ())
literalPattern = undefined

atVariablePattern :: Parser (Pattern () ())
atVariablePattern = do
  void (char '@')
  var <- name
  pure (PAtVariable () (Label () var))

-- asPattern :: Parser (Pattern () ())
-- asPattern = do
--  p <- patternParser
--  void (lexeme "as")
--  n <- name
--  pure (PAs () (Label () n) p)

constructorPattern :: Parser (Pattern () ())
constructorPattern = do
  name <- constructor
  ps <- option [] (parens (commaSep1 patternParser))
  pure (PConstructor () (Label () name) ps)
