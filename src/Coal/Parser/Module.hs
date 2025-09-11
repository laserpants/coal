{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Module (parseModule) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Language.Module
import Coal.Parser
import Coal.Parser.Identifier
import Coal.Parser.Module.Definition
import Coal.Parser.Symbol
import Extra (Name)
import Text.Megaparsec
import Text.Megaparsec.Char (upperChar)

parseModulePath :: Parser [Name]
parseModulePath = identifier upperChar `sepBy1` symbol "."

parseModuleExports :: Parser [Name]
parseModuleExports = option ["*"] (parens (commaSep name))

parseModule :: Parser (Module Metadata o ())
parseModule = do
  lexeme_ "module"
  Module . Path
    <$> parseModulePath
    <*> parseModuleExports
    <*> braces (some parseDefinition)
    <* eof
