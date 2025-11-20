{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Type (parseType, parseKind) where

import Coal.Language
import qualified Coal.Language.Type.Row as Row
import Coal.Parser.Core
import Coal.Parser.Identifier
import Coal.Parser.Symbol
import Coal.Parser.Utils (fieldList)
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Text.Megaparsec (option, optional, try, (<|>))

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

parseIntrinsicType :: Parser (Type Parameter ())
parseIntrinsicType = TIntrinsic <$> parseIntrinsic

parseAtomType :: Parser (Type Parameter ())
parseAtomType =
  try parseTypeApplication
    <|> try parseTupleType
    <|> parseRecordType
    <|> parseIntrinsicType
    <|> parseTypeParameter
    <|> parens parseType

parseType :: Parser (Type Parameter ())
parseType = makeExprParser parseAtomType typeOperator

{-# INLINE parseTypeParameter #-}
parseTypeParameter :: Parser (Type Parameter ())
parseTypeParameter = TVariable . Parameter () <$> name

{-# INLINE parseTypeConstructor #-}
parseTypeConstructor :: Parser (Type Parameter ())
parseTypeConstructor = TConstructor () <$> constructor

parseTupleType :: Parser (Type Parameter ())
parseTupleType = do
  ts <- parens (nonEmpty (commaSep2 parseType))
  pure $ TApplication () (TConstructor () (tupleTypeCons (length ts))) ts

parseRecordType :: Parser (Type Parameter ())
parseRecordType =
  braces $ do
    fields <- optional (fieldList parseType ":")
    let dict = maybe mempty Map.fromList fields
    param <- optional rest
    pure (TRecord (TRow (Row.fromDictionary dict (maybe RNil RVariable param))))
 where
  rest = pipe >> Parameter () <$> name

parseTypeApplication :: Parser (Type Parameter ())
parseTypeApplication = do
  t0 <- parseTypeConstructor <|> parseTypeParameter
  ts <- option [] (angleBrackets (commaSep1 parseType))
  case ts of
    t : ts1 ->
      pure (TApplication () t0 (t :| ts1))
    [] ->
      pure t0

typeOperator :: [[Operator Parser (Type Parameter ())]]
typeOperator = [[InfixR (TArrow <$ symbol "->")]]

parseKind :: Parser Kind
parseKind = makeExprParser (symbol "*" $> KType) kindOperator

kindOperator :: [[Operator Parser Kind]]
kindOperator = [[InfixR (KArrow <$ symbol "->")]]
