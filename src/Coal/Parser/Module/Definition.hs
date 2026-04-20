{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Coal.Parser.Module.Definition

Parsers for top-level module definitions.

Handles function definitions, type definitions, type aliases, trait
declarations, instances, imports, and top-level folds.
-}
module Coal.Parser.Module.Definition (parseDefinition) where

import Coal.Compiler.Metadata (Metadata (..))
import Coal.Language
import Coal.Language.Definition
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (Path))
import Coal.Parser.Core (Parser, lexeme, lexeme_, nonEmpty, nonEmptyOr)
import Coal.Parser.Expression (parseExpression)
import Coal.Parser.Identifier
import Coal.Parser.Metadata (withMetadata)
import Coal.Parser.Pattern (parsePattern, parseUnitPattern)
import Coal.Parser.Symbol
import Coal.Parser.Type (parseType)
import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Extras (Name, (<$$>))
import Text.Megaparsec
import Text.Megaparsec.Char (upperChar)

parseDefinition :: Parser (Definition Metadata () ())
parseDefinition =
  parseImport
    <|> try (parseFunctionGroup name)
    <|> parseFunctionDefinition name
    <|> parseLetDefinition name
    <|> try parseTypeAlias
    <|> parseTypeDefinition
    <|> parseTraitDefinition
    <|> parseTraitInstance
    <|> parseTopLevelFold

parseTypeAlias :: Parser (Definition Metadata () ())
parseTypeAlias = do
  start <- getSourcePos
  lexeme_ "type"
  lexeme_ "alias"
  n <- constructor
  ps <- option [] (angleBrackets (commaSep1 (Parameter () <$> name)))
  symbol_ "="
  t <- parseType
  end <- getSourcePos
  pure (DTypeAlias (Metadata start end) n (AliasDefinition ps t))

parseTraitDefinition :: Parser (Definition Metadata () ())
parseTraitDefinition = do
  start <- getSourcePos
  lexeme_ "trait"
  n <- constructor
  t <- angleBrackets parseParameter
  ts <- option [] (lexeme_ "with" *> commaSep1 (parseTrait parseParameter))
  end <- getSourcePos
  ds <- braces (some ((,) <$> name <*> (symbol_ ":" *> parseType)))
  pure (DTrait (Metadata start end) n (TraitDefinition (Metadata start end) n ts t (toEntry <$> toScheme <$$> ds)))

toEntry :: (Name, Scheme Parameter () ParameterizedType) -> TraitDefinitionInterfaceEntry ()
toEntry = uncurry TraitDefinitionInterfaceEntry

parseParameter :: Parser (Parameter ())
parseParameter = Parameter () <$> name

parseTraitInstance :: Parser (Definition Metadata () ())
parseTraitInstance = do
  start <- getSourcePos
  lexeme_ "instance"
  n <- constructor
  t <- angleBrackets parseType
  end <- getSourcePos
  ts <- option [] (lexeme_ "with" *> commaSep1 (parseTrait parseParameter))
  ds <- braces (some parseMethod)
  pure (DInstance (Metadata start end) (InstanceDefinition (Metadata start end) n ts t ds))
 where
  methodName = backtickName <|> name
  parseMethod =
    try (parseFunctionGroup methodName)
      <|> parseFunctionDefinition methodName
      <|> parseLetDefinition methodName

parseTrait :: Parser p -> Parser (Trait p)
parseTrait p = Trait <$> constructor <*> angleBrackets p

parseParameterList :: Parser [Parameter ()]
parseParameterList = angleBrackets (commaSep1 (Parameter () <$> name))

parseTypeDefinition :: Parser (Definition Metadata () ())
parseTypeDefinition = do
  start <- getSourcePos
  lexeme_ "type"
  n <- constructor
  ps <- option [] parseParameterList
  end <- getSourcePos
  cs <- option [] (symbol_ "=" *> parseConstructor n ps `sepBy1` symbol_ "|")
  pure (DType (Metadata start end) n (TypeDefinition ps cs))

parseConstructor :: Name -> [Parameter ()] -> Parser (DataConstructor Parameter () (Type Parameter ()))
parseConstructor tn qs = do
  n <- constructor
  ps <- option [] (parens (commaSep1 parseType))
  pure (DataConstructor n (length ps) (Forall (Set.fromList qs) mempty (foldr TArrow t0 ps)))
 where
  t0 =
    case qs of
      [] ->
        TConstructor () tn
      a : as ->
        applyTypeArgs
          ()
          (TConstructor () tn)
          (TVariable <$> (a :| as))

parseImportAtom :: Parser (Import Metadata)
parseImportAtom =
  parseTypeImport
    <|> parseTypeImport
    <|> parseNameImport

parseTypeImport :: Parser (Import Metadata)
parseTypeImport = do
  start <- getSourcePos
  name_ <- constructor
  names <- option ["*"] (parens (commaSep1 (name <|> constructor)))
  end <- getSourcePos
  pure (TypeImport (Metadata start end) name_ names)

parseNameImport :: Parser (Import Metadata)
parseNameImport = do
  start <- getSourcePos
  n <- backtickString <|> name <|> identifier upperChar
  end <- getSourcePos
  pure (NameImport (Metadata start end) n)

parseImport :: Parser (Definition Metadata o ())
parseImport = do
  start <- getSourcePos
  lexeme_ "import"
  try (parseQualifiedImport start) <|> parseNormalImport start

parseQualifiedImport :: SourcePos -> Parser (Definition Metadata o ())
parseQualifiedImport start = do
  lexeme_ "namespace"
  path <- identifier upperChar `sepBy1` symbol "."
  end <- getSourcePos
  pure (DNamespaceImport (Metadata start end) (Path path))

parseNormalImport :: SourcePos -> Parser (Definition Metadata o ())
parseNormalImport start = do
  path <- (lexeme "Builtin$" <|> identifier upperChar) `sepBy1` symbol "."
  names <- parens (commaSep parseImportAtom)
  end <- getSourcePos
  pure (DImport (Metadata start end) (Path path) names)

parseFunctionGroup :: Parser Name -> Parser (Definition Metadata () ())
parseFunctionGroup parseName = do
  start <- getSourcePos
  fn <- lexeme_ "fun" *> parseName
  ann <- optional parseAnnotation
  fns <- some (void pipe *> parseGroupFunctionDefinition ann)
  end <- getSourcePos
  case fns of
    [] -> fail "Empty list"
    fs -> pure (DFunctionGroup (Metadata start end) fn fs)

parseGroupFunctionDefinition :: Maybe ParameterizedType -> Parser (FunctionDefinition Metadata () ())
parseGroupFunctionDefinition ann = do
  start <- getSourcePos
  args <- nonEmptyOr parseUnitPattern (commaSep parsePattern)
  expr <- symbol_ "=>" *> parseExpression
  end <- getSourcePos
  pure (FunctionDefinition (Metadata start end) (With [] <$> ann) (With [] ()) args expr)

parseFunctionDefinition :: Parser Name -> Parser (Definition Metadata () ())
parseFunctionDefinition parseName = do
  start <- getSourcePos
  fn <- lexeme_ "fun" *> parseName
  args <- parens (nonEmptyOr parseUnitPattern (commaSep parsePattern))
  ann <- optional parseAnnotation
  end <- getSourcePos
  expr <- symbol_ "=" *> parseExpression
  pure (DFunction (Metadata start end) fn (FunctionDefinition (Metadata start end) (With [] <$> ann) (With [] ()) args expr))

parseLetDefinition :: Parser Name -> Parser (Definition Metadata () ())
parseLetDefinition parseName = do
  start <- getSourcePos
  c <- lexeme_ "let" *> parseName
  ann <- optional parseAnnotation
  end <- getSourcePos
  expr <- symbol_ "=" *> parseExpression
  pure (DLet (Metadata start end) c (LetDefinition (Metadata start end) (With [] <$> ann) (With [] ()) expr))

parseTopLevelFold :: Parser (Definition Metadata () ())
parseTopLevelFold = do
  start <- getSourcePos
  n <- lexeme_ "fold" *> name
  ann <- optional parseAnnotation
  end <- getSourcePos
  cs <- try (nonEmpty (some parseTopLevelFoldClause)) -- <|> braces (nonEmpty (some parseTopLevelFoldClause))
  pure (DFold (Metadata start end) n (FoldDefinition (Metadata start end) (With [] <$> ann) cs))

parseTopLevelFoldClause :: Parser (Clause Metadata () ())
parseTopLevelFoldClause =
  withMetadata $ do
    p <- symbol_ "|" *> parsePattern
    cs <- nonEmpty (some parseChoice)
    pure (\loc -> EClause loc p cs)
 where
  parseChoice =
    withMetadata $ do
      symbol_ "=>"
      e <- parseExpression
      pure (\loc -> CPlain loc [] e)

{-# INLINE parseAnnotation #-}
parseAnnotation :: Parser (Type Parameter ())
parseAnnotation = symbol_ ":" *> parseType
