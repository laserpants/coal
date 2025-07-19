{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Type where

import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Lang.Common.List1 (NonEmpty (..))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Parser
import Noll.Parser.Identifier
import Noll.Parser.Symbol
import Text.Megaparsec (option, optional, try, (<|>))
import Text.Megaparsec.Char (char)

intrinsicParser :: Parser (Intrinsic (Type Parameter ()))
intrinsicParser = 
  lexeme "int32" $> IInt32
    <|> lexeme "int64" $> IInt64

typeConstructor :: Parser (Type Parameter ())
typeConstructor = TConstructor () <$> go 
  where
    go = lexeme "list" <|> constructor

typeApplication = do
  t0 <- typeConstructor
  xx <- option [] (parens (commaSep1 typeParser))
  case xx of
    t : ts ->
      pure (TApplication () t0 (t :| ts))
    [] ->
      pure t0

typeParameter = Parameter () <$> name

-- TODO
typeParser :: Parser (Type Parameter ())
typeParser = makeExprParser go operator
  where
    go = do
      try typeApplication
        <|> (TIntrinsic <$> intrinsicParser)
        <|> (TVariable <$> typeParameter)

operator :: [[Operator Parser (Type Parameter ())]]
operator = [[InfixR (TArrow <$ symbol "->")]]
