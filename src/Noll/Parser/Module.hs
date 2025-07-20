{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Module (
  parseModule,
  parseFunctionDefinition,
  parseTraitInstance,
  parseTraitDefinition,
  parseTypeDefinition,
  parseConstantDefinition,
) where

import Lang.Common.List1 (NonEmpty (..))
import Lang.Utils (Name)
import Noll.Language
import Noll.Module
import Noll.Parser
import Noll.Parser.Expression (parseExpression)
import Noll.Parser.Identifier
import Noll.Parser.Pattern (parsePattern)
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
    <|> parseTraitInstance

parseTraitDefinition :: Parser (Definition () o ())
parseTraitDefinition = do
  lexeme_ "trait"
  n <- constructor
  t <- parens (TVariable <$> parseTypeParameter)
  ds <- braces (semicolonSep1 ((,) <$> name <*> (symbol_ ":" *> parseType)))
  -- TODO
  pure (DTrait n [] t ds)

parseTraitInstance :: Parser (Definition () o ())
parseTraitInstance = do
  lexeme_ "instance"
  n <- constructor
  t <- parens parseType
  ds <- braces (semicolonSep1 parseDefinition)
  -- TODO
  pure (DInstance n t ds)

parseTypeDefinition :: Parser (Definition () o ())
parseTypeDefinition = do
  lexeme_ "type"
  n <- constructor
  ps <- option [] (parens (commaSep1 (Parameter () <$> name)))
  cs <- symbol_ "=" *> parseConstructor n ps `sepBy1` symbol_ "|"
  pure (DType n ps cs)

parseConstructor :: Name -> [Parameter ()] -> Parser (Constructor Parameter () (Type Parameter ()))
parseConstructor c qs = do
  n <- constructor
  ps <- option [] (parens (commaSep1 parseType))
  pure (Constructor n (length ps) (Forall (Set.fromList qs) [] (foldr TArrow t0 ps)))
 where
  t0 =
    case qs of
      [] ->
        TConstructor () c
      a : as ->
        TApplication
          ()
          (TConstructor () c)
          (TVariable <$> (a :| as))

parseImport :: Parser (Definition () o ())
parseImport =
  endingWithSemicolon $ do
    lexeme_ "import"
    path <- (lexeme "Core$" <|> identifier upperChar) `sepBy1` symbol "."
    names <- option ["*"] (parens (commaSep (backtickString <|> name)))
    pure (DImport (Path path) names)

parseFunctionDefinition :: Parser (Definition () o ())
parseFunctionDefinition =
  endingWithSemicolon $ do
    fn <- lexeme_ "fn" *> name
    args <- parens (nonEmptyOr (pure $ PLiteral () LUnit :| []) (commaSep parsePattern))
    withAnnotation $ do
      expr <- symbol_ "=" *> parseExpression
      pure (DFunction fn (Function () (With [] ()) args expr))

parseConstantDefinition :: Parser (Definition () o ())
parseConstantDefinition =
  endingWithSemicolon $ do
    c <- name
    withAnnotation $ do
      expr <- symbol_ "=" *> parseExpression
      pure (DConstant c (Constant () (With [] ()) expr))

withAnnotation :: Parser (Definition () o ()) -> Parser (Definition () o ())
withAnnotation p = do
  ann <- optional (symbol_ ":" *> parseType)
  d <- p
  case ann of
    Nothing ->
      pure d
    Just t ->
      pure (DAnnotation (With [] t) d)

parseModule :: Parser (Module () o ())
parseModule = do
  lexeme_ "module"
  path <- identifier upperChar `sepBy1` symbol "."
  exps <- option ["*"] (parens (commaSep name))
  ds <- braces (some parseDefinition) <* eof
  pure (Module (Path path) exps ds)
