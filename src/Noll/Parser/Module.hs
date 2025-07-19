{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Noll.Parser.Module where -- (moduleParser, functionParser, constantParser) where

import Lang.Common.List1 (NonEmpty (..))
import Noll.Language
import Noll.Module
import Noll.Parser
import Noll.Parser.Expression (expressionParser)
import Noll.Parser.Identifier
import Noll.Parser.Pattern (patternParser)
import Noll.Parser.Symbol
import Noll.Parser.Type
import Text.Megaparsec
import Text.Megaparsec.Char (upperChar)
import Lang.Utils (Name)

import qualified Data.Set as Set

definitionParser :: Parser (Definition () o ())
definitionParser =
  importParser
    <|> functionParser
    <|> constantParser
    <|> typeDefinitionParser

typeDefinitionParser :: Parser (Definition () o ())
typeDefinitionParser = do
  lexeme_ "type"
  n <- constructor
  ps <- option [] (parens (commaSep1 (Parameter () <$> name)))
  symbol_ "="
  cs <- ctor n `sepBy1` symbol_ "|"
  pure (DType n ps cs)

toScheme :: Name -> [Type Parameter ()] -> Scheme Parameter () (Type Parameter ())
toScheme n ps = Forall (Set.fromList (params =<< ps)) [] (foldr TArrow q ps)
  where
    q =
      case ps of
        [] ->
          TConstructor () n
        a : as ->
          TApplication
            ()
            (TConstructor () n)
            (a :| as)

params :: Type Parameter () -> [Parameter ()]
params =
  \case
    TVariable p ->
      [p]
    TApplication _ t ts ->
      params t <> concat (params <$> ts)
    TArrow t1 t2 ->
      params t1 <> params t2
    TConstructor{} ->
      []
    TIntrinsic t ->
      error "TODO"
    TRow r ->
      error "TODO"
    TAlias _ _ t ->
      params t

ctor :: Name -> Parser (Constructor Parameter () (Type Parameter ()))
ctor x = do
  n <- constructor
  as <- option [] (parens (commaSep1 typeParser))
  pure (Constructor n (length as) (toScheme x as))

importParser :: Parser (Definition () o ())
importParser = do
  lexeme_ "import"
  path <- (lexeme "Core$" <|> identifier upperChar) `sepBy1` symbol "."
  names <- option ["*"] (parens (commaSep (backtickString <|> name)))
  symbol_ ";"
  pure (DImport (Path path) names)

functionParser :: Parser (Definition () o ())
functionParser = do
  lexeme_ "fn"
  fn <- name
  args <- parens (commaSep patternParser)
  ann <- optional (symbol_ ":" *> typeParser)
  symbol_ "="
  expr <- expressionParser
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

constantParser :: Parser (Definition () o ())
constantParser = do
  c <- name
  ann <- optional (symbol_ ":" *> typeParser)
  symbol_ "="
  expr <- expressionParser
  symbol_ ";"
  let e = DConstant c (Constant () (With [] ()) expr)
  case ann of
    Nothing ->
      pure e
    Just t ->
      pure (DAnnotation (With [] t) e)

moduleParser :: Parser (Module () o ())
moduleParser = do
  lexeme_ "module"
  path <- identifier upperChar `sepBy1` symbol "."
  exps <- option ["*"] (parens (commaSep name))
  b <- braces (many definitionParser)
  eof
  pure (Module (Path path) exps b)
