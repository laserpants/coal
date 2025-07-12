{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Module where

import Control.Monad (void)
import Lang.Common.List1 (NonEmpty (..))
import Noll.Language
import Noll.Module
import Noll.Module.Definition (Path (..))
import Noll.Parser
import Noll.Parser.Expression (expressionParser)
import Noll.Parser.Identifier
import Noll.Parser.Pattern (patternParser)
import Noll.Parser.Type
import Noll.Parser.Symbol
import Text.Megaparsec
import Text.Megaparsec.Char (upperChar)

definitionParser :: Parser (Definition () o ())
definitionParser =
  --  functionParser
  void (lexeme "foo") >> pure (DImport (Path []) [])

functionParser :: Parser (Definition () o ())
functionParser = do
  fn <- name
  args <- parens (commaSep patternParser)
  ann <- optional (void (symbol ":") *> typeParser)
  void (symbol "=")
  expr <- expressionParser
  void (symbol ";")
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

moduleParser :: Parser (Module () o ())
moduleParser = do
  void (lexeme "module")
  path <- identifier upperChar `sepBy` symbol "."
  exps <- option ["*"] (parens (commaSep name))
  b <- many definitionParser
  eof
  pure (Module (Path path) exps b)
