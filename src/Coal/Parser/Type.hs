{-# LANGUAGE OverloadedStrings #-}

-- FIXME
module Coal.Parser.Type (parseType, parseKind) where

import Coal.Language
import qualified Coal.Language.Type.Row as Row
import Coal.Parser
import Coal.Parser.Identifier
import Coal.Parser.Symbol
import Coal.Parser.Utils (fieldList)
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Text.Megaparsec (option, optional, try, (<|>))

parseInt32 :: Parser (Intrinsic (Type Parameter ()))
parseInt32 = lexeme "int32" $> IInt32

parseInt64 :: Parser (Intrinsic (Type Parameter ()))
parseInt64 = lexeme "int64" $> IInt64

parseBool :: Parser (Intrinsic (Type Parameter ()))
parseBool = lexeme "bool" $> IBool

parseChar :: Parser (Intrinsic (Type Parameter ()))
parseChar = lexeme "char" $> IChar

parseDouble :: Parser (Intrinsic (Type Parameter ()))
parseDouble = lexeme "double" $> IDouble

parseFloat :: Parser (Intrinsic (Type Parameter ()))
parseFloat = lexeme "float" $> IFloat

parseBignum :: Parser (Intrinsic (Type Parameter ()))
parseBignum = lexeme "bignum" $> IBignum

parseNat :: Parser (Intrinsic (Type Parameter ()))
parseNat = lexeme "nat" $> INat

parseString :: Parser (Intrinsic (Type Parameter ()))
parseString = lexeme "string" $> IString

parseUnit :: Parser (Intrinsic (Type Parameter ()))
parseUnit = lexeme "unit" $> IUnit

parseVoid :: Parser (Intrinsic (Type Parameter ()))
parseVoid = lexeme "void" $> IVoid

parseIntrinsic :: Parser (Intrinsic (Type Parameter ()))
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
    pure (TIntrinsic (IRecord (TRow (Row.fromDictionary dict (maybe RNil RVariable param)))))
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
