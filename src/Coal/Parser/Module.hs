{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Module (
  parseModule,
  parseFunctionDefinition,
  parseTraitInstance,
  parseTraitDefinition,
  parseTypeDefinition,
  parseConstantDefinition,
) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Common.List1 (NonEmpty (..))
import Coal.Language
import Coal.Language.Module
import Coal.Parser
import Coal.Parser.Expression (parseExpression)
import Coal.Parser.Identifier
import Coal.Parser.Pattern (parsePattern, parseUnitPattern)
import Coal.Parser.Symbol
import Coal.Parser.Type
import Extra (Name)
import Text.Megaparsec
import Text.Megaparsec.Char (upperChar)

import qualified Data.Set as Set

parseDefinition :: Parser (Definition Metadata o ())
parseDefinition =
  parseImport
    <|> parseFunctionDefinition
    <|> parseConstantDefinition
    <|> parseTypeDefinition
    <|> parseTraitDefinition
    <|> parseTraitInstance

parseTraitDefinition :: Parser (Definition Metadata o ())
parseTraitDefinition = do
  lexeme_ "trait"
  n <- constructor
  t <- parens parseParameter
  ds <- braces (semicolonSep1 ((,) <$> name <*> (symbol_ ":" *> parseType)))
  -- TODO
  pure (DTrait n [] t ds)

parseParameter :: Parser (Parameter Kind)
parseParameter = do
  n <- name
  k <- option KType (symbol_ ":" *> parseKind)
  pure (Parameter k n)

parseTraitInstance :: Parser (Definition Metadata o ())
parseTraitInstance = do
  lexeme_ "instance"
  n <- constructor
  t <- parens parseType
  ds <- braces (some parseDefinition)
  -- ds <- braces (semicolonSep1 ((,) <$> name <*> (symbol_ "=" *> parseDefinition)))
  -- TODO
  pure (DInstance n t ds)

parseTypeDefinition :: Parser (Definition Metadata o ())
parseTypeDefinition = do
  lexeme_ "type"
  n <- constructor
  ps <- option [] (parens (commaSep1 (Parameter () <$> name)))
  cs <- symbol_ "=" *> parseConstructor n ps `sepBy1` symbol_ "|"
  pure (DType n ps cs)

parseConstructor :: Name -> [Parameter ()] -> Parser (Constructor Parameter () (Type Parameter ()))
parseConstructor tn qs = do
  n <- constructor
  ps <- option [] (parens (commaSep1 parseType))
  pure (Constructor n (length ps) (Forall (Set.fromList qs) [] (foldr TArrow t0 ps)))
 where
  t0 =
    case qs of
      [] ->
        TConstructor () tn
      a : as ->
        TApplication
          ()
          (TConstructor () tn)
          (TVariable <$> (a :| as))

parseImport :: Parser (Definition Metadata o ())
parseImport =
  endingWithSemicolon $ do
    lexeme_ "import"
    path <- (lexeme "Core$" <|> identifier upperChar) `sepBy1` symbol "."
    names <- option ["*"] (parens (commaSep (backtickString <|> name <|> identifier upperChar)))
    pure (DImport (Path path) names)

parseFunctionDefinition :: Parser (Definition Metadata o ())
parseFunctionDefinition =
  endingWithSemicolon $ do
    start <- getSourcePos
    fn <- lexeme_ "fun" *> name
    args <- parens (nonEmptyOr parseUnitPattern (commaSep parsePattern))
    withAnnotation $ do
      end <- getSourcePos
      expr <- symbol_ "=" *> parseExpression
      pure (DFunction fn (Function (Metadata start end) (With [] ()) args expr))

parseConstantDefinition :: Parser (Definition Metadata o ())
parseConstantDefinition =
  endingWithSemicolon $ do
    start <- getSourcePos
    c <- lexeme_ "let" *> name
    withAnnotation $ do
      end <- getSourcePos
      expr <- symbol_ "=" *> parseExpression
      pure (DConstant c (Constant (Metadata start end) (With [] ()) expr))

withAnnotation :: Parser (Definition Metadata o ()) -> Parser (Definition Metadata o ())
withAnnotation p = do
  ann <- optional (symbol_ ":" *> parseType)
  d <- p
  case ann of
    Nothing ->
      pure d
    Just t ->
      pure (DAnnotation (With [] t) d)

parseModule :: Parser (Module Metadata o ())
parseModule = do
  lexeme_ "module"
  path <- identifier upperChar `sepBy1` symbol "."
  exps <- option ["*"] (parens (commaSep name))
  ds <- braces (some parseDefinition) <* eof
  pure (Module (Path path) exps ds)
