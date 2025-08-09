{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Type (parseType, parseKind) where

import Coal.Common.List1 (NonEmpty (..))
import Coal.Parser.Utils (fieldList)
import Coal.Language
import Coal.Parser
import Coal.Parser.Identifier
import Coal.Parser.Symbol
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Extra (Name)
import Text.Megaparsec (option, try, (<|>))

import qualified Coal.Language.Type.Row as Row
import qualified Data.Map.Strict as Map

parseIntrinsicType :: Parser (Type Parameter ())
parseIntrinsicType = TIntrinsic <$> parser
 where
  parser =
    (lexeme "int32" $> IInt32)
      <|> (lexeme "int64" $> IInt64)
      <|> (lexeme "bool" $> IBool)
      <|> (lexeme "char" $> IChar)
      <|> (lexeme "double" $> IDouble)
      <|> (lexeme "float" $> IFloat)
      <|> (lexeme "bignum" $> IBignum)
      <|> (lexeme "nat" $> INat)
      <|> (lexeme "string" $> IString)
      <|> (lexeme "unit" $> IUnit)
      <|> (lexeme "void" $> IVoid)

{-# INLINE parseTypeParameter #-}
parseTypeParameter :: Parser (Type Parameter ())
parseTypeParameter = TVariable . Parameter () <$> name

{-# INLINE parseTypeConstructor #-}
parseTypeConstructor :: Parser (Type Parameter ())
parseTypeConstructor = TConstructor () <$> constructor

parseRecordType :: Parser (Type Parameter ())
parseRecordType = do
  fields <- braces (fieldList parseType ":")
  let dict = pure <$> Map.fromList fields
  -- TODO
  pure (TIntrinsic (IRecord (TRow (Row.fromDictionary dict RNil))))

parseTypeApplication :: Parser (Type Parameter ())
parseTypeApplication = do
  t0 <- parseTypeConstructor <|> parseTypeParameter
  ts <- option [] (parens (commaSep1 parseType))
  case ts of
    t : ts1 ->
      pure (TApplication () t0 (t :| ts1))
    [] ->
      pure t0

parseType :: Parser (Type Parameter ())
parseType = makeExprParser go typeOperator
 where
  go =
    try parseTypeApplication
      <|> parseRecordType
      <|> parseIntrinsicType
      <|> parseTypeParameter
      <|> parens parseType

typeOperator :: [[Operator Parser (Type Parameter ())]]
typeOperator = [[InfixR (TArrow <$ symbol "->")]]

parseKind :: Parser Kind
parseKind = makeExprParser (symbol "*" $> KType) kindOperator

kindOperator :: [[Operator Parser Kind]]
kindOperator = [[InfixR (KArrow <$ symbol "->")]]
