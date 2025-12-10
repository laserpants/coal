{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Module (parseModule) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Language.Module
import Coal.Parser.Core
import Coal.Parser.Identifier
import Coal.Parser.Module.Definition
import Coal.Parser.Symbol
import Extras (Name)
import Text.Megaparsec
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
  pure (ExportType (Metadata start end) name_ names)

parseNameExport :: Parser (Export Metadata)
parseNameExport = do
  start <- getSourcePos
  n <- name
  end <- getSourcePos
  pure (ExportName (Metadata start end) n)

{-# INLINE parseModuleExports #-}
parseModuleExports :: Parser [Export Metadata]
parseModuleExports = option [ExportAll] (parens (commaSep parseExportAtom))

parseModule :: Parser (Module Metadata o ())
parseModule = do
  lexeme_ "module"
  Module . Path
    <$> parseModulePath
    <*> parseModuleExports
    <*> braces (many parseDefinition)
    <* eof
