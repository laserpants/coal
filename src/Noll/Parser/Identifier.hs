{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Identifier where

import Noll.Parser
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar)

import qualified Data.Text as Text

{-# INLINE underscore #-}
underscore :: Parser Char
underscore = char '_'

{-# INLINE validChar #-}
validChar :: Parser Char
validChar = alphaNumChar <|> underscore

{-# INLINE name #-}
name :: Parser Text
name = identifier (lowerChar <|> underscore)

withInitial :: Parser [Char] -> Parser Char -> Parser Text
withInitial chrs chr = Text.pack <$> cons chr chrs

identifier :: Parser Char -> Parser Text
identifier initial = word $ many validChar `withInitial` initial
