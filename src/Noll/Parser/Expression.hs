{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Expression where

import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Noll.Language
import Noll.Parser
import Noll.Parser.Symbol
import Lang.Common.List1 (NonEmpty (..))
import Lang.Label (Label (..))
import Text.Megaparsec ((<|>), try)
import Noll.Parser.Identifier

expressionParser :: Parser (Expression () ())
expressionParser = makeExprParser go operator
  where
    go =
      try functionCall
--      <|> letExpression
      <|> literalExpression
--      <|> variableExpression     
      <|> parens expressionParser

functionCall :: Parser (Expression () ())
functionCall = do 
  fn <- try (parens expressionParser) <|> EVariable () . Label () <$> name
  arg : args <- parens (commaSep1 expressionParser)
  pure (EApplication () () fn (arg :| args))

letExpression :: Parser (Expression () ())
letExpression = undefined

foldExpression :: Parser (Expression () ())
foldExpression = undefined

variableExpression :: Parser (Expression () ())
variableExpression = undefined

literalExpression :: Parser (Expression () ())
literalExpression = undefined

unaryOperator :: UnaryOperator -> Expression () () -> Expression () ()
unaryOperator op e1 = 
  EApplication
    ()
    ()
    (EUnaryOperator () () op)
    (e1 :| [])

binaryOperator :: BinaryOperator -> Expression () () -> Expression () () -> Expression () ()
binaryOperator op e1 e2 = 
  EApplication
    ()
    ()
    (EBinaryOperator () () op)
    (e1 :| [e2])

fixity8 = []

fixity7 = 
  [ InfixL (binaryOperator OMultiplication <$ symbol "*")
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

