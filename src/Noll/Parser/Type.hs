{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Type where

import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Lang.Common.List1 (NonEmpty (..))
import Noll.Language
import Noll.Parser
import Noll.Parser.Identifier
import Noll.Parser.Symbol
import Text.Megaparsec (option, try, (<|>))

intrinsicParser :: Parser (Intrinsic (Type Parameter ()))
intrinsicParser =
  lexeme "int32" $> IInt32
    <|> lexeme "int64" $> IInt64
    <|> lexeme "bool" $> IBool
    <|> lexeme "char" $> IChar
    <|> lexeme "double" $> IDouble
    <|> lexeme "float" $> IFloat
    <|> lexeme "bignum" $> IBignum
    <|> lexeme "nat" $> INat
    <|> lexeme "string" $> IString
    <|> lexeme "unit" $> IUnit
    <|> lexeme "void" $> IVoid

typeConstructor :: Parser (Type Parameter ())
typeConstructor = TConstructor () <$> constructor

-- where
--  go = lexeme "list" <|> constructor

typeApplication = do
  t0 <- typeConstructor
  xx <- option [] (parens (commaSep1 typeParser))
  case xx of
    t : ts ->
      pure (TApplication () t0 (t :| ts))
    [] ->
      pure t0

typeParameter :: Parser (Parameter ())
typeParameter = Parameter () <$> name

-- TODO
typeParser :: Parser (Type Parameter ())
typeParser = makeExprParser go operator
 where
  go = do
    try typeApplication
      <|> (lexeme_ "list" *> (TIntrinsic . IList <$> parens typeParser))
      <|> (TIntrinsic <$> intrinsicParser)
      <|> (TVariable <$> typeParameter)

operator :: [[Operator Parser (Type Parameter ())]]
operator = [[InfixR (TArrow <$ symbol "->")]]
