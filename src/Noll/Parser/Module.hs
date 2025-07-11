{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Module where

import Control.Monad (void)
import Lang.Common.List1 (NonEmpty (..))
import Noll.Language
import Noll.Module
import Noll.Module.Definition (Path (..))
import Noll.Parser
import Noll.Parser.Expression
import Noll.Parser.Identifier
import Noll.Parser.Pattern
import Noll.Parser.Symbol
import Text.Megaparsec
import Text.Megaparsec.Char (upperChar)

definitionParser :: Parser (Definition () o ())
definitionParser =
  --  functionParser
  void (lexeme "foo") >> pure (DImport (Path []) [])

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
  b <- many definitionParser
  eof
  pure (Module (Path path) exps b)
