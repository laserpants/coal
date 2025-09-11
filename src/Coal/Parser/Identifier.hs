module Coal.Parser.Identifier (
  underscore,
  validChar,
  name,
  constructor,
  withInitial,
  identifier,
  backtickString,
) where

import Coal.Parser
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import Extra (Name)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, upperChar)

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

backtickString :: Parser Name
backtickString =
  lexeme $ do
    void (char '`')
    s <- takeWhileP (Just "Non-backtick character") (/= '`')
    void (char '`')
    pure s
