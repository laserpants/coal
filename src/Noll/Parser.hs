{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, newline, spaceChar)

import qualified Data.Text as Text
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text

type ParserError = ParseErrorBundle Text Void
