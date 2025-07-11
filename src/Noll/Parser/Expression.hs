{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Expression where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Lang.Common.List1 (NonEmpty (..))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Parser
import Noll.Parser.Identifier
import Noll.Parser.Pattern (patternParser)
import Noll.Parser.Symbol
import Text.Megaparsec (some, try, (<|>))

import qualified Text.Megaparsec.Char.Lexer as Lexer

expressionParser :: Parser (Expression () ())
expressionParser = makeExprParser go operator
 where
  go =
    try functionCall
      <|> intExpression
      <|> foldExpression
      <|> variableExpression
      <|> parens expressionParser

functionCall :: Parser (Expression () ())
functionCall = do
  fn <- try (parens expressionParser) <|> EVariable () . Label () <$> name
  arg : args <- parens (commaSep1 expressionParser)
  pure (EApplication () () fn (arg :| args))

letExpression :: Parser (Expression () ())
letExpression = undefined

choice :: Parser (Choice Expression () ())
choice = do
  e <- expressionParser
  -- TODO
  pure (CPlain () [] e)

clause :: Parser (Clause () ())
clause = do
  void (symbol "|")
  p <- patternParser
  void (symbol "=>")
  c : cs <- some choice
  pure (EClause () p (c :| cs))

foldExpression :: Parser (Expression () ())
foldExpression = do
  void $ lexeme "fold"
  expr : exprs <- parens (commaSep1 expressionParser)
  c : cs <- braces (some clause)
  pure (EFold () () (expr :| exprs) (c :| cs) Nothing)

variableExpression :: Parser (Expression () ())
variableExpression = do
  var <- name
  pure (EVariable () (Label () var))

intExpression :: Parser (Expression () ())
intExpression = do
  n <- Lexer.signed spaces (lexeme Lexer.decimal)
  pure $
    EApplication
      ()
      ()
      (EVariable () (Label () "from_int32"))
      (ELiteral () (LInt32 n) :| [])

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
