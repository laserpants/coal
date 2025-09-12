{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Primitive (parsePrimitive) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Language.Expression (Expression (..))
import Coal.Language.Primitive
import Coal.Parser
import Coal.Parser.Metadata
import Control.Monad (void)
import Data.Char (ord)
import Data.Functor (($>))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Text.Megaparsec
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

parsePrimitive :: Parser (Expression Metadata ())
parsePrimitive =
  withMetadata $ do
    lit <- parseAtom
    pure (`ELiteral` lit)

parseTrue :: Parser Primitive
parseTrue = lexeme_ "true" $> LBool True

parseFalse :: Parser Primitive
parseFalse = lexeme_ "false" $> LBool False

squote :: Parser Char
squote = char '\''

parseChar :: Parser Primitive
parseChar = do
  lexeme $ do
    void squote
    ch <- Lexer.charLiteral
    void squote
    pure (LChar (fromIntegral (ord ch)))

dquote :: Parser Char
dquote = char '"'

parseString :: Parser Primitive
parseString = do
  lexeme $ do
    void dquote
    chars <- manyTill Lexer.charLiteral dquote
    pure (LString (Text.encodeUtf8 (Text.pack chars)))

parseFloat :: Parser Primitive
parseFloat = LFloat <$> lexeme (Lexer.float <* (char 'f' <|> char 'F'))

parseDouble :: Parser Primitive
parseDouble = LDouble <$> lexeme (Lexer.float :: Parser Double)
