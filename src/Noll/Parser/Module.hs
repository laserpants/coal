{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Module (
  parseModule, 
  parseFunctionDefinition, 
  parseInstanceDefinition, 
  parseTraitDefinition, 
  parseTypeDefinition, 
  parseConstantDefinition
 ) where

import Lang.Common.List1 (NonEmpty (..))
import Lang.Utils (Name)
import Noll.Language
import Noll.Module
import Noll.Parser
import Noll.Parser.Expression (parseExpression)
import Noll.Parser.Identifier
import Noll.Parser.Pattern (patternParser)
import Noll.Parser.Symbol
import Noll.Parser.Type
import Text.Megaparsec
import Text.Megaparsec.Char (upperChar)

import qualified Data.Set as Set

parseDefinition :: Parser (Definition () o ())
parseDefinition =
  parseImport
    <|> parseFunctionDefinition
    <|> parseConstantDefinition
    <|> parseTypeDefinition
    <|> parseTraitDefinition
    <|> parseInstanceDefinition

parseTraitDefinition :: Parser (Definition () o ())
parseTraitDefinition = do
  lexeme_ "trait"
  n <- constructor
  t <- parens (TVariable <$> typeParameter)
  xx <- braces (semicolonSep1 sig)
  -- TODO
  pure (DTrait n [] t xx)

sig :: Parser (Name, Type Parameter ())
sig = do
  n <- name
  symbol_ ":"
  t <- typeParser
  pure (n, t)

parseInstanceDefinition :: Parser (Definition () o ())
parseInstanceDefinition = do
  lexeme_ "instance"
  n <- constructor
  t <- parens typeParser
  xx <- braces (semicolonSep1 parseDefinition)
  -- TODO
  pure (DInstance n t xx)

parseTypeDefinition :: Parser (Definition () o ())
parseTypeDefinition = do
  lexeme_ "type"
  c <- constructor
  ps <- option [] (parens (commaSep1 (Parameter () <$> name)))
  symbol_ "="
  cs <- ctor c ps `sepBy1` symbol_ "|"
  pure (DType c ps cs)

ctor :: Name -> [Parameter ()] -> Parser (Constructor Parameter () (Type Parameter ()))
ctor c qs = do
  n <- constructor
  ps <- option [] (parens (commaSep1 typeParser))
  pure (Constructor n (length ps) (toScheme ps))
 where
    toScheme ps = Forall (Set.fromList qs) [] (foldr TArrow qq ps)
    qq =
      case qs of
        [] ->
          TConstructor () c
        a : as ->
          TApplication
            ()
            (TConstructor () c)
            (TVariable <$> (a :| as))

parseImport :: Parser (Definition () o ())
parseImport = do
  lexeme_ "import"
  path <- (lexeme "Core$" <|> identifier upperChar) `sepBy1` symbol "."
  names <- option ["*"] (parens (commaSep (backtickString <|> name)))
  symbol_ ";"
  pure (DImport (Path path) names)

parseFunctionDefinition :: Parser (Definition () o ())
parseFunctionDefinition = do
  lexeme_ "fn"
  fn <- name
  args <- parens (commaSep patternParser)
  ann <- optional (symbol_ ":" *> typeParser)
  symbol_ "="
  expr <- parseExpression
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

parseConstantDefinition :: Parser (Definition () o ())
parseConstantDefinition = do
  c <- name
  ann <- optional (symbol_ ":" *> typeParser)
  symbol_ "="
  expr <- parseExpression
  symbol_ ";"
  let e = DConstant c (Constant () (With [] ()) expr)
  case ann of
    Nothing ->
      pure e
    Just t ->
      pure (DAnnotation (With [] t) e)

parseModule :: Parser (Module () o ())
parseModule = do
  lexeme_ "module"
  path <- identifier upperChar `sepBy1` symbol "."
  exps <- option ["*"] (parens (commaSep name))
  b <- braces (many parseDefinition)
  eof
  pure (Module (Path path) exps b)
