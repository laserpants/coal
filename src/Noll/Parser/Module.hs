{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Module (moduleParser, functionParser, constantParser) where

import Lang.Common.List1 (NonEmpty (..))
import Noll.Language
import Noll.Module
import Noll.Parser
import Noll.Parser.Expression (expressionParser)
import Noll.Parser.Identifier
import Noll.Parser.Pattern (patternParser)
import Noll.Parser.Symbol
import Noll.Parser.Type
import Text.Megaparsec
import Text.Megaparsec.Char (upperChar)

definitionParser :: Parser (Definition () o ())
definitionParser =
  importParser
    <|> functionParser
    <|> constantParser

importParser :: Parser (Definition () o ())
importParser = do
  lexeme_ "import"
  path <- (lexeme "Core$" <|> identifier upperChar) `sepBy1` symbol "."
  names <- option ["*"] (parens (commaSep (backtickString <|> name)))
  symbol_ ";"
  pure (DImport (Path path) names)

functionParser :: Parser (Definition () o ())
functionParser = do
  lexeme_ "fn"
  fn <- name
  args <- parens (commaSep patternParser)
  ann <- optional (symbol_ ":" *> typeParser)
  symbol_ "="
  expr <- expressionParser
  symbol_ ";"
  let args' =
        case args of
          [] ->
            PLiteral () LUnit :| []
          a : as ->
            a :| as
  let f = DFunction fn (Function () (With [] ()) args' expr)
  case ann of
    Nothing ->
      pure f
    Just t ->
      pure (DAnnotation (With [] t) f)

constantParser :: Parser (Definition () o ())
constantParser = do
  c <- name
  ann <- optional (symbol_ ":" *> typeParser)
  symbol_ "="
  expr <- expressionParser
  symbol_ ";"
  let e = DConstant c (Constant () (With [] ()) expr)
  case ann of
    Nothing ->
      pure e
    Just t ->
      pure (DAnnotation (With [] t) e)

moduleParser :: Parser (Module () o ())
moduleParser = do
  lexeme_ "module"
  path <- identifier upperChar `sepBy1` symbol "."
  exps <- option ["*"] (parens (commaSep name))
  b <- braces (many definitionParser)
  eof
  pure (Module (Path path) exps b)
