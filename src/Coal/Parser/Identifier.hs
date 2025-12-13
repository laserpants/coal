{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Identifier (
  underscore,
  validChar,
  name,
  backtickName,
  constructor,
  magicConstructor,
  withInitial,
  identifier,
  backtickString,
) where

import Coal.Parser.Core (Parser, cons, lexeme, word)
import Coal.Parser.Symbol (symbol)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import Extras (Name)
import Text.Megaparsec (MonadParsec (takeWhileP), many, (<|>))
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

backtickName :: Parser Text
backtickName = do
  s <- backtickString
  pure ("(" <> s <> ")")

{-# INLINE constructor #-}
constructor :: Parser Text
constructor = identifier upperChar

magicConstructor :: Parser Text
magicConstructor = do
  s <- symbol "@"
  n <- constructor
  pure (s <> n)

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
