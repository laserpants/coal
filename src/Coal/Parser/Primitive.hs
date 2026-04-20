{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Coal.Parser.Primitive

Parsers for primitive literal values.

Handles booleans, characters, strings, integers, and floating-point literals.
-}
module Coal.Parser.Primitive (parseAtom, parsePrimitive) where

import Coal.Compiler.Metadata (Metadata (..))
import Coal.Language.Expression (Expression (..))
import Coal.Language.Primitive
import Coal.Parser.Core (Parser, lexeme, lexeme_)
import Coal.Parser.Metadata (withMetadata)
import Control.Monad (void)
import Data.Char (ord)
import Data.Functor (($>))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Extras.Text.Megaparsec.Char (doubleQuote, singleQuote)
import Text.Megaparsec (MonadParsec (try), manyTill, (<|>))
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as Lexer

parseAtom :: Parser Primitive
parseAtom =
  parseTrue
    <|> parseFalse
    <|> parseChar
    <|> parseString
    <|> try parseFloat
    <|> try parseDouble

parsePrimitive :: Parser (Expression Metadata () ())
parsePrimitive =
  withMetadata $ do
    lit <- parseAtom
    pure (`ELiteral` lit)

parseTrue :: Parser Primitive
parseTrue = lexeme_ "true" $> LBool True

parseFalse :: Parser Primitive
parseFalse = lexeme_ "false" $> LBool False

parseChar :: Parser Primitive
parseChar = do
  lexeme $ do
    void singleQuote
    ch <- Lexer.charLiteral
    void singleQuote
    pure (LChar (fromIntegral (ord ch)))

parseString :: Parser Primitive
parseString = do
  lexeme $ do
    void doubleQuote
    chars <- manyTill Lexer.charLiteral doubleQuote
    pure (LString (Text.encodeUtf8 (Text.pack chars)))

parseFloat :: Parser Primitive
parseFloat = LFloat <$> lexeme (Lexer.float <* (char 'f' <|> char 'F'))

parseDouble :: Parser Primitive
parseDouble = LDouble <$> lexeme (Lexer.float :: Parser Double)
