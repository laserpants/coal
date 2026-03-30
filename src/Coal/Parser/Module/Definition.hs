{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Module.Definition (parseDefinition) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Language
import Coal.Language.Module
import Coal.Parser.Core
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

parseDefinition :: Parser (Definition Metadata o ())
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

parseTypeAlias :: Parser (Definition Metadata o ())
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

parseTraitDefinition :: Parser (Definition Metadata o ())
parseTraitDefinition = do
  start <- getSourcePos
  lexeme_ "trait"
  n <- constructor
  t <- angleBrackets parseParameter
  ts <- option [] (lexeme_ "with" *> commaSep1 (parseTrait parseParameter))
  end <- getSourcePos
  ds <- braces (some ((,) <$> name <*> (symbol_ ":" *> parseType)))
  pure (DTrait (Metadata start end) n (TraitDefinition ts t (toScheme <$$> ds)))

parseParameter :: Parser (Parameter ())
parseParameter = Parameter () <$> name

parseTraitInstance :: Parser (Definition Metadata o ())
parseTraitInstance = do
  start <- getSourcePos
  lexeme_ "instance"
  n <- constructor
  t <- angleBrackets parseType
  end <- getSourcePos
  ts <- option [] (lexeme_ "with" *> commaSep1 (parseTrait parseType))
  ds <- braces (some parseMethod)
  pure (DInstance (Metadata start end) n (InstanceDefinition ts t ds))
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

parseTypeDefinition :: Parser (Definition Metadata o ())
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
  pure (DQualifiedImport (Metadata start end) (Path path))

parseNormalImport :: SourcePos -> Parser (Definition Metadata o ())
parseNormalImport start = do
  path <- (lexeme "Builtin$" <|> identifier upperChar) `sepBy1` symbol "."
  names <- parens (commaSep parseImportAtom)
  end <- getSourcePos
  pure (DImport (Metadata start end) (Path path) names)

parseFunctionGroup :: Parser Name -> Parser (Definition Metadata o ())
parseFunctionGroup parseName = do
  start <- getSourcePos
  fn <- lexeme_ "fun" *> parseName
  ann <- optional parseAnnotation
  fns <- some (void pipe *> parseGroupFunctionDefinition ann)
  end <- getSourcePos
  case fns of
    [] -> fail "Empty list"
    f : fs -> pure (DFunction (Metadata start end) fn (f :| fs) [])

parseGroupFunctionDefinition :: Maybe ParameterizedType -> Parser (FunctionDefinition Metadata ())
parseGroupFunctionDefinition ann = do
  start <- getSourcePos
  args <- nonEmptyOr parseUnitPattern (commaSep parsePattern)
  expr <- symbol_ "=" *> parseExpression
  end <- getSourcePos
  pure (FunctionDefinition (Metadata start end) (With [] <$> ann) (With [] ()) args expr)

-- TODO: DRY
parseFunctionDefinition :: Parser Name -> Parser (Definition Metadata o ())
parseFunctionDefinition parseName = do
  start <- getSourcePos
  fn <- lexeme_ "fun" *> parseName
  args <- parens (nonEmptyOr parseUnitPattern (commaSep parsePattern))
  ann <- optional parseAnnotation
  end <- getSourcePos
  expr <- symbol_ "=" *> parseExpression
  ws <- option [] parseWhereClauses
  pure (DFunction (Metadata start end) fn (FunctionDefinition (Metadata start end) (With [] <$> ann) (With [] ()) args expr :| []) ws)

parseWhereClauses :: Parser [Definition Metadata o ()]
parseWhereClauses = lexeme_ "where" *> braces (some (parseFunctionDefinition name))

parseLetDefinition :: Parser Name -> Parser (Definition Metadata o ())
parseLetDefinition parseName = do
  start <- getSourcePos
  c <- lexeme_ "let" *> parseName
  ann <- optional parseAnnotation
  end <- getSourcePos
  expr <- symbol_ "=" *> parseExpression
  ws <- option [] parseWhereClauses
  pure (DConstant (Metadata start end) c (ConstantDefinition (Metadata start end) (With [] <$> ann) (With [] ()) expr) ws)

parseTopLevelFold :: Parser (Definition Metadata o ())
parseTopLevelFold = do
  start <- getSourcePos
  n <- lexeme_ "fold" *> name
  ann <- optional parseAnnotation
  end <- getSourcePos
  cs <- try (nonEmpty (some parseTopLevelFoldClause)) -- <|> braces (nonEmpty (some parseTopLevelFoldClause))
  pure (DFold (Metadata start end) n (FoldDefinition (With [] <$> ann) cs))

parseTopLevelFoldClause :: Parser (Clause Metadata () ())
parseTopLevelFoldClause =
  withMetadata $ do
    p <- symbol_ "|" *> parsePattern
    cs <- nonEmpty (some parseChoice)
    pure (\loc -> EClause loc p cs)
 where
  parseChoice =
    withMetadata $ do
      symbol_ "="
      e <- parseExpression
      pure (\loc -> CPlain loc [] e)

{-# INLINE parseAnnotation #-}
parseAnnotation :: Parser (Type Parameter ())
parseAnnotation = symbol_ ":" *> parseType

-- TODO: move
toScheme :: Type Parameter () -> Scheme Parameter () ParameterizedType
toScheme t = Forall (Set.fromList (params t)) mempty t

class Parameterized p where
  params :: p -> [Parameter ()]

instance (Parameterized p) => Parameterized [p] where
  params = concatMap params

instance (Parameterized p) => Parameterized (NonEmpty p) where
  params = concatMap params

instance Parameterized (Type Parameter ()) where
  params =
    \case
      TVariable p ->
        params p
      TApplication _ t ts ->
        params t <> params ts
      TArrow t1 t2 ->
        params t1 <> params t2
      TIntrinsic t ->
        params t
      TRecord t ->
        params t
      TRow r ->
        params r
      TAlias _ _ t ->
        params t
      TConstructor{} ->
        []

instance Parameterized Intrinsic where
  params _ = []

instance Parameterized (Row Parameter () (Type Parameter ())) where
  params =
    \case
      RVariable p ->
        params p
      RExtend _ t r ->
        params t <> params r
      RNil ->
        []

instance Parameterized (Parameter ()) where
  params = return
