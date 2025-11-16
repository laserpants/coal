module Coal.Kernel.Parser.Identifier (name, constructor, identifier) where

import Coal.Kernel.Parser (Parser, cons, option, word)
import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec (many)
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, upperChar)

{-# INLINE underscore #-}
underscore :: Parser Char
underscore = char '_'

{-# INLINE validChar #-}
validChar :: Parser Char
validChar = alphaNumChar <|> char '.' <|> char '$' <|> underscore

withInitial :: Parser [Char] -> Parser Char -> Parser Text
withInitial chrs chr = Text.pack <$> cons chr chrs

identifier :: Parser Char -> Parser Text
identifier initial =
  word $ do
    lhs <- option "" (many (char '$'))
    rhs <- many validChar `withInitial` initial
    pure (Text.pack lhs <> rhs)

{-# INLINE name #-}
name :: Parser Text
name = identifier (lowerChar <|> underscore)

{-# INLINE constructor #-}
constructor :: Parser Text
constructor = identifier upperChar
