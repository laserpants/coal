{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Identifier where

import Data.Text (Text)
import Noll.Parser
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, upperChar)

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

{-# INLINE constructor #-}
constructor :: Parser Text
constructor = identifier upperChar

withInitial :: Parser [Char] -> Parser Char -> Parser Text
withInitial chrs chr = Text.pack <$> cons chr chrs

identifier :: Parser Char -> Parser Text
identifier initial = word $ many validChar `withInitial` initial
