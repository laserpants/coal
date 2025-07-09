{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Module where

import Lang.Common.List1 (NonEmpty (..))
import Control.Monad (void)
import Noll.Parser
import Noll.Parser.Identifier
import Noll.Module 
import Noll.Language
import Noll.Parser.Pattern
import Noll.Parser.Expression
import Text.Megaparsec
import Noll.Parser.Symbol
import Noll.Module.Definition (Path (..))
import Text.Megaparsec.Char (upperChar)

definitionParser :: Parser (Definition () o ())
definitionParser =
--  functionParser
  lexeme "foo" >> pure (DImport (Path []) [])

functionParser :: Parser (Definition () o ())
functionParser = do
  void (lexeme "fun")
  fn <- name
  arg : args <- parens (commaSep1 patternParser)
  void (symbol "=")
  expr <- expressionParser
  void (symbol ";")
  pure (DFunction fn (Function () (With [] ()) (arg :| args) expr))

moduleParser :: Parser (Module () o ())
moduleParser = do
  void (lexeme "module")
  path <- identifier upperChar `sepBy` symbol "."
  exps <- option ["*"] (parens (commaSep name))
  b <- braces (many definitionParser)
  eof
  pure (Module (Path path) exps b)
