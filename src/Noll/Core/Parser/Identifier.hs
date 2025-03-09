module Noll.Core.Parser.Identifier (name, constructor) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Noll.Core.Parser (Parser, cons, word)
import Text.Megaparsec (many)
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, upperChar)

import qualified Data.Text as Text

{-# INLINE underscore #-}
underscore :: Parser Char
underscore = char '_'

{-# INLINE validChar #-}
validChar :: Parser Char
validChar = alphaNumChar <|> underscore

withInitial :: Parser [Char] -> Parser Char -> Parser Text
withInitial chrs chr = Text.pack <$> cons chr chrs

identifier :: Parser Char -> Parser Text
identifier initial = word (many validChar `withInitial` initial)

{-# INLINE name #-}
name :: Parser Text
name = identifier (lowerChar <|> underscore)

{-# INLINE constructor #-}
constructor :: Parser Text
constructor = identifier (upperChar <|> char '$')
