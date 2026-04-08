{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Module (parseModule) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Language.Module.Export (Export (..))
import Coal.Language.Module.Path (Path (Path))
import Coal.Parser.Core (Parser, lexeme_)
import Coal.Parser.Identifier (constructor, identifier, name)
import Coal.Parser.Module.Definition (parseDefinition)
import Coal.Parser.Symbol
import Coal.ProtoLanguage.ProtoModule
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
  pure (TypeExport (Metadata start end) name_ names)

parseNameExport :: Parser (Export Metadata)
parseNameExport = do
  start <- getSourcePos
  n <- name
  end <- getSourcePos
  pure (NameExport (Metadata start end) n)

-- {-# INLINE parseModuleExports #-}
-- parseModuleExports :: Parser [Export Metadata]
-- parseModuleExports = option [] (parens (commaSep parseExportAtom))

parseModuleExports :: Parser (ModuleExportList Metadata)
parseModuleExports = option ExportAll (parens (Exports <$> commaSep parseExportAtom))

-- parseModule :: Parser (Module Metadata o ())
-- parseModule = do
--  lexeme_ "module"
--  Module . Path
--    <$> parseModulePath
--    <*> parseModuleExports
--    <*> braces (many parseDefinition)
--    <* eof

parseModule :: Parser (ProtoModule Metadata () ())
parseModule = do
  lexeme_ "module"
  ProtoModule . Path
    <$> parseModulePath
    <*> parseModuleExports
    <*> braces (many parseDefinition)
    <* eof
