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
import Coal.Language
import Coal.Language.Module
import Coal.Language.Module.Cotype (Cotype (..))
import Coal.Parser
import Coal.Parser.Expression (parseExpression, parseMatchClause)
import Coal.Parser.Identifier
import Coal.Parser.Pattern (parsePattern, parseUnitPattern)
import Coal.Parser.Symbol
import Coal.Parser.Type
import Coal.Parser.Utils (fieldListWithKey)
import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty (..))
import Extra (Name)
import Text.Megaparsec
import Text.Megaparsec.Char (upperChar)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

parseDefinition :: Parser (Definition Metadata o ())
parseDefinition =
  parseImport
    <|> parseFunctionDefinition
    <|> parseConstantDefinition
    <|> parseTypeDefinition
    <|> parseCodataDefinition
    <|> parseTraitDefinition
    <|> parseTraitInstance
    <|> parseTopLevelFold
    <|> parseTopLevelUnfold

parseTraitDefinition :: Parser (Definition Metadata o ())
parseTraitDefinition = do
  start <- getSourcePos
  lexeme_ "trait"
  n <- constructor
  t <- angleBrackets parseParameter
  end <- getSourcePos
  ds <- braces (some ((,) <$> name <*> (symbol_ ":" *> parseType)))
  -- TODO
  pure (DTrait (Metadata start end) n [] t ds)

parseParameter :: Parser (Parameter Kind)
parseParameter = do
  n <- name
  k <- option KType (symbol_ ":" *> parseKind)
  pure (Parameter k n)

parseTraitInstance :: Parser (Definition Metadata o ())
parseTraitInstance = do
  start <- getSourcePos
  lexeme_ "instance"
  n <- constructor
  t <- angleBrackets parseType
  end <- getSourcePos
  ts <- option [] (lexeme_ "with" *> commaSep1 parseTrait)
  ds <- braces (some parseDefinition)
  pure (DInstance (Metadata start end) n ts t ds)

parseTrait :: Parser (Trait (Type Parameter ()))
parseTrait = do
  n <- constructor
  t <- angleBrackets parseType
  pure (Trait n t)

parseTypeDefinition :: Parser (Definition Metadata o ())
parseTypeDefinition = do
  start <- getSourcePos
  lexeme_ "type"
  n <- constructor
  -- TODO: DRY
  ps <- option [] (angleBrackets (commaSep1 (Parameter () <$> name)))
  end <- getSourcePos
  cs <- symbol_ "=" *> parseConstructor n ps `sepBy1` symbol_ "|"
  pure (DType (Metadata start end) n ps cs)

parseCodataDefinition :: Parser (Definition Metadata o ())
parseCodataDefinition = do
  start <- getSourcePos
  lexeme_ "cotype"
  n <- constructor
  ps <- option [] (angleBrackets (commaSep1 (Parameter () <$> name)))
  end <- getSourcePos
  symbol_ "="
  fields <- braces (fieldListWithKey constructor parseType ":")
  pure (DCotype (Metadata start end) n (Cotype ps fields))

parseConstructor :: Name -> [Parameter ()] -> Parser (DataConstructor Parameter () (Type Parameter ()))
parseConstructor tn qs = do
  n <- constructor
  ps <- option [] (parens (commaSep1 parseType))
  pure (DataConstructor n (length ps) (Forall (Set.fromList qs) [] (foldr TArrow t0 ps)))
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
parseImport = do
  start <- getSourcePos
  lexeme_ "import"
  path <- (lexeme "Core$" <|> identifier upperChar) `sepBy1` symbol "."
  names <- option ["*"] (parens (commaSep (backtickString <|> name <|> identifier upperChar)))
  end <- getSourcePos
  pure (DImport (Metadata start end) (Path path) names)

parseFunctionDefinition :: Parser (Definition Metadata o ())
parseFunctionDefinition = do
  start <- getSourcePos
  fn <- lexeme_ "fun" *> name
  args <- parens (nonEmptyOr parseUnitPattern (commaSep parsePattern))
  ann <- optional (symbol_ ":" *> parseType)
  --  withAnnotation $ do
  end <- getSourcePos
  expr <- symbol_ "=" *> parseExpression
  ws <- option [] parseWhereClauses
  pure (DFunction (Metadata start end) fn (Function (Metadata start end) (With [] <$> ann) (With [] ()) args expr) ws)

parseWhereClauses :: Parser [Definition Metadata o ()]
parseWhereClauses = lexeme_ "where" *> braces (some parseFunctionDefinition)

parseConstantDefinition :: Parser (Definition Metadata o ())
parseConstantDefinition = do
  start <- getSourcePos
  c <- lexeme_ "let" *> name
  ann <- optional (symbol_ ":" *> parseType)
  --  withAnnotation $ do
  end <- getSourcePos
  expr <- symbol_ "=" *> parseExpression
  ws <- option [] parseWhereClauses
  pure (DConstant (Metadata start end) c (Constant (Metadata start end) (With [] <$> ann) (With [] ()) expr) ws)

parseTopLevelFold :: Parser (Definition Metadata o ())
parseTopLevelFold = do
  start <- getSourcePos
  n <- lexeme_ "fold" *> name
  ann <- symbol_ ":" *> parseType
  end <- getSourcePos
  cs <- braces (nonEmpty (some parseMatchClause))
  pure (DFold (Metadata start end) n (With [] ann) cs Nothing)

parseTopLevelUnfold :: Parser (Definition Metadata o ())
parseTopLevelUnfold = do
  start <- getSourcePos
  n <- lexeme_ "unfold" *> name
  ann <- symbol_ ":" *> parseType
  end <- getSourcePos
  ps <- parens (nonEmpty (commaSep1 parsePattern))
  fields <- braces $ do
    void $ optional (symbol ",")
    fieldListWithKey constructor parseExpression "="
  pure (DUnfold (Metadata start end) n (With [] ann) ps (Map.fromList fields) Nothing)

-- withAnnotation :: Parser (Definition Metadata o ()) -> Parser (Definition Metadata o ())
-- withAnnotation p = do
--  start <- getSourcePos
--  ann <- optional (symbol_ ":" *> parseType)
--  end <- getSourcePos
--  d <- p
--  case ann of
--    Nothing ->
--      pure d
--    Just t ->
--      pure (DAnnotation (Metadata start end) (With [] t) d)

parseModule :: Parser (Module Metadata o ())
parseModule = do
  lexeme_ "module"
  path <- identifier upperChar `sepBy1` symbol "."
  exps <- option ["*"] (parens (commaSep name))
  ds <- braces (some parseDefinition) <* eof
  pure (Module (Path path) exps ds)
