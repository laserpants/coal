{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Expression where

import Control.Monad.Combinators.Expr
import Noll.Language
import Noll.Parser
import Noll.Parser.Symbol
import Text.Megaparsec
import Noll.Parser.Identifier

expressionParser :: Parser (Expression () ())
expressionParser = makeExprParser go operator
  where
    go =
      variableExpression     

letExpression :: Parser (Expression () ())
letExpression = undefined

foldExpression :: Parser (Expression () ())
foldExpression = undefined

variableExpression :: Parser (Expression () ())
variableExpression = undefined

applicationExpression :: Parser (Expression () ())
applicationExpression = undefined

literalExpression :: Parser (Expression () ())
literalExpression = undefined

binaryOperator = undefined

fixity8 = []

fixity7 = 
  [ InfixL (binaryOperator ((), OMultiplication) <$ symbol "*")
  ]

fixity6 = []
fixity5 = []
fixity4 = []
fixity3 = []
fixity2 = []

operator :: [[Operator Parser (Expression () ())]]
operator =
  [ fixity8
  , fixity7
  , fixity6
  , fixity4
  , fixity3
  , fixity2
  ]

