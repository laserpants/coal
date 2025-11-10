{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Module.Definition (parseDefinition) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Language
import Coal.Language.Module
import Coal.Parser.Core
import Coal.Parser.Expression (parseExpression, parseMatchClause)
import Coal.Parser.Identifier
import Coal.Parser.Pattern (parsePattern, parseUnitPattern)
import Coal.Parser.Symbol
import Coal.Parser.Type
import Coal.Parser.Utils (fieldList, fieldListWithKey)
import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Extras (Name, (<$$>))
import Text.Megaparsec
import Text.Megaparsec.Char (upperChar)

parseDefinition :: Parser (Definition Metadata o ())
parseDefinition =
  parseImport
    <|> try parseFunctionGroup
    <|> parseFunctionDefinition
    <|> parseLetDefinition
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
  pure (DTrait (Metadata start end) n (TraitDef [] t (toScheme <$$> ds)))

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
  pure (DInstance (Metadata start end) n (InstanceDef ts t ds))

parseTrait :: Parser (Trait (Type Parameter ()))
parseTrait = Trait <$> constructor <*> angleBrackets parseType

parseParameterList :: Parser [Parameter ()]
parseParameterList = angleBrackets (commaSep1 (Parameter () <$> name))

parseTypeDefinition :: Parser (Definition Metadata o ())
parseTypeDefinition = do
  start <- getSourcePos
  lexeme_ "type"
  n <- constructor
  ps <- option [] parseParameterList
  end <- getSourcePos
  cs <- symbol_ "=" *> parseConstructor n ps `sepBy1` symbol_ "|"
  pure (DType (Metadata start end) n (TypeDef ps cs))

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

parseCodataDefinition :: Parser (Definition Metadata o ())
parseCodataDefinition = do
  start <- getSourcePos
  lexeme_ "cotype"
  n <- constructor
  ps <- option [] parseParameterList
  end <- getSourcePos
  symbol_ "="
  let
    t0 =
      case ps of
        [] ->
          TConstructor () n
        a : as ->
          TApplication
            ()
            (TConstructor () n)
            (TVariable <$> (a :| as))
  ts <- braces (fieldListWithKey constructor parseType ":")
  pure (DCotype (Metadata start end) n (CotypeDef ps (toAccessor ps t0 <$> ts)))

-- TODO
-- toAccessor :: Type Parameter () -> (Name, Type Parameter ()) -> CodataAccessor Parameter () (Type Parameter ())
toAccessor ps t0 (n, t) = CodataAccessor n (Forall (Set.fromList ps) [] (t0 `TArrow` t))

parseImport :: Parser (Definition Metadata o ())
parseImport = do
  start <- getSourcePos
  lexeme_ "import"
  path <- (lexeme "Builtin$" <|> identifier upperChar) `sepBy1` symbol "."
  names <- option ["*"] (parens (commaSep (backtickString <|> name <|> identifier upperChar)))
  end <- getSourcePos
  pure (DImport (Metadata start end) (Path path) names)

parseFunctionGroup :: Parser (Definition Metadata o ())
parseFunctionGroup = do
  start <- getSourcePos
  fn <- lexeme_ "fun" *> name
  ann <- optional parseAnnotation
  fns <- some (void pipe *> parseFunctionDef ann)
  end <- getSourcePos
  case fns of
    [] -> fail "Empty list"
    f : fs -> pure (DFunction (Metadata start end) fn (f :| fs) [])

-- TODO: DRY
parseFunctionDefinition :: Parser (Definition Metadata o ())
parseFunctionDefinition = do
  start <- getSourcePos
  fn <- lexeme_ "fun" *> name
  args <- parens (nonEmptyOr parseUnitPattern (commaSep parsePattern))
  ann <- optional parseAnnotation
  end <- getSourcePos
  expr <- symbol_ "=" *> parseExpression
  ws <- option [] parseWhereClauses
  pure (DFunction (Metadata start end) fn (FunctionDef (Metadata start end) (With [] <$> ann) (With [] ()) args expr :| []) ws)

parseFunctionDef :: Maybe ParameterizedType -> Parser (FunctionDef Metadata ())
parseFunctionDef ann = do
  start <- getSourcePos
  args <- parens (nonEmptyOr parseUnitPattern (commaSep parsePattern))
  expr <- symbol_ "=" *> parseExpression
  end <- getSourcePos
  pure (FunctionDef (Metadata start end) (With [] <$> ann) (With [] ()) args expr)

parseWhereClauses :: Parser [Definition Metadata o ()]
parseWhereClauses = lexeme_ "where" *> braces (some parseFunctionDefinition)

parseLetDefinition :: Parser (Definition Metadata o ())
parseLetDefinition = do
  start <- getSourcePos
  c <- lexeme_ "let" *> name
  ann <- optional parseAnnotation
  end <- getSourcePos
  expr <- symbol_ "=" *> parseExpression
  ws <- option [] parseWhereClauses
  pure (DConstant (Metadata start end) c (ConstantDef (Metadata start end) (With [] <$> ann) (With [] ()) expr) ws)

parseTopLevelFold :: Parser (Definition Metadata o ())
parseTopLevelFold = do
  start <- getSourcePos
  n <- lexeme_ "fold" *> name
  ann <- parseAnnotation
  end <- getSourcePos
  cs <- braces (nonEmpty (some parseMatchClause))
  pure (DFold (Metadata start end) n (FoldDef (With [] ann) cs Nothing))

parseTopLevelUnfold :: Parser (Definition Metadata o ())
parseTopLevelUnfold = do
  start <- getSourcePos
  n <- lexeme_ "unfold" *> name
  ps <- parens (nonEmpty (commaSep1 parsePattern))
  ann <- parseAnnotation
  fields <- braces $ do
    void $ optional (symbol ",")
    fieldListWithKey (magicConstructor <|> constructor) parseExpression "="
  end <- getSourcePos
  pure (DUnfold (Metadata start end) n (UnfoldDef (With [] ann) ps (Map.fromList fields) Nothing))

{-# INLINE parseAnnotation #-}
parseAnnotation :: Parser (Type Parameter ())
parseAnnotation = symbol_ ":" *> parseType

-- TODO: move
toScheme :: Type Parameter () -> Scheme Parameter () ParameterizedType
toScheme t = Forall (Set.fromList (params t)) [] t

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
      TRow r ->
        params r
      TAlias _ _ t ->
        params t
      TConstructor{} ->
        []

instance Parameterized (Intrinsic (Type Parameter ())) where
  params =
    \case
      IRecord t ->
        params t
      _ ->
        []

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
