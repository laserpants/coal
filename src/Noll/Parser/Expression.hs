{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Expression where -- (expressionParser) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Lang.Common.List1 (NonEmpty (..))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Parser
import Noll.Parser.Identifier
import Noll.Parser.Pattern (patternParser)
import Noll.Parser.Symbol
import Noll.Parser.Type
import Text.Megaparsec (some, try, (<|>))

import qualified Text.Megaparsec.Char.Lexer as Lexer

expressionParser :: Parser (Expression () ())
expressionParser = makeExprParser go operator
 where
  go =
    try functionCall
      <|> intExpression
      <|> literalExpression
      <|> foldExpression
      <|> matchExpression
      <|> letExpression
      <|> variableExpression
      <|> parens expressionParser

functionCall :: Parser (Expression () ())
functionCall = do
  fn <- try (parens expressionParser) <|> EVariable () . Label () <$> name
  arg : args <- parens (commaSep1 expressionParser)
  pure (EApplication () () fn (arg :| args))

patternBinding :: Parser (Binding Expression () ())
patternBinding = BPattern () <$> (patternParser <* symbol "=") <*> expressionParser

functionBinding :: Parser (Binding Expression () ())
functionBinding = undefined

letBinding :: Parser (Binding Expression () ())
letBinding = patternBinding <|> functionBinding

letExpression :: Parser (Expression () ())
letExpression = do
  void $ lexeme "let"
  bs <- semicolonSep1 letBinding
  void $ lexeme "in"
  e <- expressionParser
  case bs of
    b : bs1 ->
      pure (ELet () (b :| bs1) e)

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
variableExpression = EVariable () . Label () <$> name

choiceParser = do
  -- TODO
  e <- expressionParser
  pure (CPlain () [] e)

matchClause = do
  void $ symbol "|"
  p <- patternParser
  void $ symbol "=>"
  cs <- some choiceParser
  case cs of
    c : cs1 ->
      pure (EClause () p (c :| cs1))

matchExpression :: Parser (Expression () ())
matchExpression = do
  void $ lexeme "match"
  e <- parens expressionParser
  cs <- braces (some matchClause)
  case cs of
    c : cs1 ->
      pure (EMatch () () e (c :| cs1))

intExpression :: Parser (Expression () ())
intExpression = do
  n <- Lexer.signed spaces (lexeme Lexer.decimal)
  pure $
    EApplication
      ()
      ()
      (EVariable () (Label () "from_int32"))
      (ELiteral () (LInt32 n) :| [])

listLiteral :: Parser (Expression () ())
listLiteral = do
  es <- brackets (commaSep expressionParser)
  pure (EListLiteral () () es)

literalExpression :: Parser (Expression () ())
literalExpression = listLiteral

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

fixity9 :: [Operator Parser (Expression () ())]
fixity9 = 
  [ -- TODO
    InfixR (binaryOperator OReverseComposition <$ symbol "<<")
  ]

fixity8 :: [Operator Parser (Expression () ())]
fixity8 = []

fixity7 :: [Operator Parser (Expression () ())]
fixity7 =
  [ InfixL (binaryOperator OMultiplication <$ symbol "*")
  ]

fixity6, fixity5, fixity4, fixity3, fixity2 :: [Operator Parser (Expression () ())]
fixity6 = []
fixity5 =
  [ InfixR (binaryOperator OListConcatenation <$ symbol "++")
  , InfixR (EListCons () () <$ symbol "::")
  ]
fixity4 = []
fixity3 = []
fixity2 = []

operator :: [[Operator Parser (Expression () ())]]
operator =
  [ fixity9
  , fixity8
  , fixity7
  , fixity6
  , fixity5
  , fixity4
  , fixity3
  , fixity2
  , [Postfix typeAnnotation]
  ]

typeAnnotation :: Parser (Expression () () -> Expression () ())
typeAnnotation = do
  symbol_ ":"
  EAnnotation () <$> typeParser
