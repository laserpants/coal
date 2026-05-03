{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
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
  pure
    ( DTypeAlias
        (Metadata start end)
        n
        AliasDefinition
          { aliasDefinitionParameters = ps
          , aliasDefinitionType = t
          }
    )

parseTraitDefinition :: Parser (Definition Metadata () ())
parseTraitDefinition = do
  start <- getSourcePos
  lexeme_ "trait"
  n <- constructor
  t <- angleBrackets parseParameter
  ts <- option [] (lexeme_ "with" *> commaSep1 (parseTrait parseParameter))
  end <- getSourcePos
  ds <- braces (some ((,) <$> name <*> (symbol_ ":" *> parseType)))
  pure
    ( DTrait
        (Metadata start end)
        n
        TraitDefinition
          { traitDefinitionMetadata = Metadata start end
          , traitDefinitionTraitName = n
          , traitDefinitionConstraints = ts
          , traitDefinitionParameter = t
          , traitDefinitionInterface = toEntry <$> toScheme <$$> ds
          }
    )

toEntry :: (Name, Scheme Parameter () ParameterizedType) -> TraitDefinitionInterfaceEntry ()
toEntry = uncurry TraitDefinitionInterfaceEntry

parseParameter :: Parser (Parameter ())
parseParameter = Parameter () <$> name

parseTraitInstance :: Parser (Definition Metadata () ())
parseTraitInstance = do
  start <- getSourcePos
  lexeme_ "instance"
  instanceDefinitionTraitName <- constructor
  instanceDefinitionType <- angleBrackets parseType
  end <- getSourcePos
  instanceDefinitionConstraints <- option [] (lexeme_ "with" *> commaSep1 (parseTrait parseParameter))
  instanceDefinitionImplementations <- braces (some parseMethod)
  pure
    ( DInstance
        (Metadata start end)
        InstanceDefinition
          { instanceDefinitionMetadata = Metadata start end
          , instanceDefinitionTraitName
          , instanceDefinitionConstraints
          , instanceDefinitionType
          , instanceDefinitionImplementations
          }
    )
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
  pure
    ( DType
        (Metadata start end)
        n
        TypeDefinition
          { typeDefinitionParameters = ps
          , typeDefinitionConstructors = cs
          }
    )

parseConstructor :: Name -> [Parameter ()] -> Parser (DataConstructor Parameter () (Type Parameter ()))
parseConstructor tn qs = do
  constructorName <- constructor
  ps <- option [] (parens (commaSep1 parseType))
  pure $
    DataConstructor
      { constructorName
      , constructorArity = length ps
      , constructorScheme = Forall (Set.fromList qs) mempty (foldr TArrow t0 ps)
      }
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
    fs ->
      pure
        ( DFunctionGroup
            (Metadata start end)
            fn
            FunctionGroupDefinition
              { functionGroupDefinitionMetadata = Metadata start end
              , functionGroupDefinitionAnnotation = With [] <$> ann
              , functionGroupDefinitionBranches = fs
              }
        )

parseGroupFunctionDefinition :: Maybe ParameterizedType -> Parser (FunctionDefinition Metadata () ())
parseGroupFunctionDefinition ann = do
  start <- getSourcePos
  functionDefinitionPatterns <- nonEmptyOr parseUnitPattern (commaSep parsePattern)
  functionDefinitionExpression <- symbol_ "=>" *> parseExpression
  end <- getSourcePos
  pure
    FunctionDefinition
      { functionDefinitionMetadata = Metadata start end
      , functionDefinitionAnnotation = With [] <$> ann
      , functionDefinitionType = With [] ()
      , functionDefinitionPatterns
      , functionDefinitionExpression
      }

parseFunctionDefinition :: Parser Name -> Parser (Definition Metadata () ())
parseFunctionDefinition parseName = do
  start <- getSourcePos
  functionDefinitionName <- lexeme_ "fun" *> parseName
  functionDefinitionPatterns <- parens (nonEmptyOr parseUnitPattern (commaSep parsePattern))
  ann <- optional parseAnnotation
  end <- getSourcePos
  functionDefinitionExpression <- symbol_ "=" *> parseExpression
  fin <- getSourcePos
  pure
    ( DFunction
        (Metadata start fin)
        functionDefinitionName
        FunctionDefinition
          { functionDefinitionMetadata = Metadata start end
          , functionDefinitionAnnotation = With [] <$> ann
          , functionDefinitionType = With [] ()
          , functionDefinitionPatterns
          , functionDefinitionExpression
          }
    )

parseLetDefinition :: Parser Name -> Parser (Definition Metadata () ())
parseLetDefinition parseName = do
  start <- getSourcePos
  letDefinitionName <- lexeme_ "let" *> parseName
  ann <- optional parseAnnotation
  end <- getSourcePos
  letDefinitionExpression <- symbol_ "=" *> parseExpression
  fin <- getSourcePos
  pure
    ( DLet
        (Metadata start fin)
        letDefinitionName
        LetDefinition
          { letDefinitionMetadata = Metadata start end
          , letDefinitionAnnotation = With [] <$> ann
          , letDefinitionType = With [] ()
          , letDefinitionExpression
          }
    )

parseTopLevelFold :: Parser (Definition Metadata () ())
parseTopLevelFold = do
  start <- getSourcePos
  foldName <- lexeme_ "fold" *> name
  ann <- optional parseAnnotation
  end <- getSourcePos
  foldDefinitionClauses <- try (nonEmpty (some parseTopLevelFoldClause))
  pure
    ( DFold
        (Metadata start end)
        foldName
        FoldDefinition
          { foldDefinitionMetadata = Metadata start end
          , foldDefinitionAnnotation = With [] <$> ann
          , foldDefinitionClauses
          }
    )

parseTopLevelFoldClause :: Parser (Clause Metadata () ())
parseTopLevelFoldClause =
  withMetadata $ do
    clausePattern <- symbol_ "|" *> parsePattern
    clauseChoices <- nonEmpty (some parseChoice)
    pure
      ( \clauseMetadata ->
          EClause
            { clauseMetadata
            , clausePattern
            , clauseChoices
            }
      )
 where
  parseChoice =
    withMetadata $ do
      symbol_ "=>"
      choiceExpression <- parseExpression
      pure
        ( \choiceMetadata ->
            CPlain
              { choiceMetadata
              , choiceGuards = []
              , choiceExpression
              }
        )

{-# INLINE parseAnnotation #-}
parseAnnotation :: Parser (Type Parameter ())
parseAnnotation = symbol_ ":" *> parseType
