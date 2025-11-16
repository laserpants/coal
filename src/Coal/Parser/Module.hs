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

{-# INLINE parseModuleExports #-}
parseModuleExports :: Parser [Name]
parseModuleExports = option ["*"] (parens (commaSep name))

parseModule :: Parser (Module Metadata o ())
parseModule = do
  lexeme_ "module"
  Module . Path
    <$> parseModulePath
    <*> parseModuleExports
    <*> braces (many parseDefinition)
    <* eof
