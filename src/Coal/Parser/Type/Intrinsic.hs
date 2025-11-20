{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Type.Intrinsic (parseIntrinsic) where

import Coal.Language (Intrinsic (..))
import Coal.Parser.Core (Parser, lexeme)
import Data.Functor (($>))
import Text.Megaparsec ((<|>))

parseIntrinsic :: Parser Intrinsic
parseIntrinsic =
  parseInt32
    <|> parseInt64
    <|> parseBool
    <|> parseChar
    <|> parseDouble
    <|> parseFloat
    <|> parseBignum
    <|> parseNat
    <|> parseString
    <|> parseUnit
    <|> parseVoid

parseInt32 :: Parser Intrinsic
parseInt32 = lexeme "int32" $> IInt32

parseInt64 :: Parser Intrinsic
parseInt64 = lexeme "int64" $> IInt64

parseBool :: Parser Intrinsic
parseBool = lexeme "bool" $> IBool

parseChar :: Parser Intrinsic
parseChar = lexeme "char" $> IChar

parseDouble :: Parser Intrinsic
parseDouble = lexeme "double" $> IDouble

parseFloat :: Parser Intrinsic
parseFloat = lexeme "float" $> IFloat

parseBignum :: Parser Intrinsic
parseBignum = lexeme "bignum" $> IBignum

parseNat :: Parser Intrinsic
parseNat = lexeme "nat" $> INat

parseString :: Parser Intrinsic
parseString = lexeme "string" $> IString

parseUnit :: Parser Intrinsic
parseUnit = lexeme "unit" $> IUnit

parseVoid :: Parser Intrinsic
parseVoid = lexeme "void" $> IVoid
