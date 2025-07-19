{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Type (parseType, parseTypeParameter) where

import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Lang.Common.List1 (NonEmpty (..))
import Noll.Language
import Noll.Parser
import Noll.Parser.Identifier
import Noll.Parser.Symbol
import Text.Megaparsec (option, try, (<|>))

parseIntrinsicType :: Parser (Intrinsic (Type Parameter ()))
parseIntrinsicType =
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
parseTypeParameter :: Parser (Parameter ())
parseTypeParameter = Parameter () <$> name

{-# INLINE parseTypeConstructor #-}
parseTypeConstructor :: Parser (Type Parameter ())
parseTypeConstructor = TConstructor () <$> constructor

parseTypeApplication :: Parser (Type Parameter ())
parseTypeApplication = do
  t0 <- parseTypeConstructor
  ts <- option [] (parens (commaSep1 parseType))
  case ts of
    t : ts1 ->
      pure (TApplication () t0 (t :| ts1))
    [] ->
      pure t0

parseType :: Parser (Type Parameter ())
parseType = makeExprParser go operator
 where
  go = do
    try parseTypeApplication
      <|> (lexeme_ "list" *> (TIntrinsic . IList <$> parens parseType))
      <|> (TIntrinsic <$> parseIntrinsicType)
      <|> (TVariable <$> parseTypeParameter)

operator :: [[Operator Parser (Type Parameter ())]]
operator = [[InfixR (TArrow <$ symbol "->")]]
