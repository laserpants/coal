{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Coal.Parser.Module

Module declaration and export list parsers.

Parses module headers including module paths and export specifications.
-}
module Coal.Parser.Module (parseModule, parseSourceFile) where

import Coal.Compiler.Metadata (Metadata (..))
import Coal.Language.Module (ExportList (..), Module (Module))
import Coal.Language.Module.Export (Export (..))
import Coal.Language.Module.Path (Path (Path))
import Coal.Parser.Core (Parser, lexeme_, spaces)
import Coal.Parser.Identifier (constructor, identifier, name)
import Coal.Parser.Module.Definition (parseDefinition)
import Coal.Parser.Symbol
import Extras (Name)
import Text.Megaparsec (
  MonadParsec (eof),
  getSourcePos,
  many,
  option,
  sepBy1,
  (<|>),
 )
import Text.Megaparsec.Char (upperChar)

{-# INLINE parseModulePath #-}
parseModulePath :: Parser [Name]
parseModulePath = identifier upperChar `sepBy1` symbol "."

parseExportAtom :: Parser (Export Metadata)
parseExportAtom =
  parseTypeExport
    <|> parseNameExport

parseTypeExport :: Parser (Export Metadata)
parseTypeExport = do
  start <- getSourcePos
  name_ <- constructor
  names <- option ["*"] (parens (commaSep1 name))
  end <- getSourcePos
  pure (TypeExport (Metadata start end) name_ names)

parseNameExport :: Parser (Export Metadata)
parseNameExport = do
  start <- getSourcePos
  n <- name
  end <- getSourcePos
  pure (NameExport (Metadata start end) n)

parseModuleExports :: Parser (ExportList Metadata)
parseModuleExports = option ExportAll (parens (Exports <$> commaSep parseExportAtom))

parseSourceFile :: Parser (Module Metadata () ())
parseSourceFile = spaces *> parseModule <* eof

parseModule :: Parser (Module Metadata () ())
parseModule = do
  lexeme_ "module"
  Module . Path
    <$> parseModulePath
    <*> parseModuleExports
    <*> braces (many parseDefinition)
    <* eof
