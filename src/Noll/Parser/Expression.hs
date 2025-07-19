{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Expression where -- (expressionParser) where

import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Lang.Common.List1 (NonEmpty (..))
import Lang.Label (Label (..))
import Lang.Utils (Name)
import Noll.Language
import Noll.Parser
import Noll.Parser.Identifier
import Noll.Parser.Pattern (patternParser)
import Noll.Parser.Symbol
import Noll.Parser.Type (typeParser)
import Text.Megaparsec (optional, some, try, (<|>))

import qualified Data.Map.Strict as Map
import qualified Text.Megaparsec.Char.Lexer as Lexer

expressionParser :: Parser (Expression () ())
expressionParser = makeExprParser go operator
 where
  go = do
    e1 <-
      try functionCall
        <|> dataConstructor
        <|> intExpression
        <|> literalExpression
        <|> foldExpression
        <|> matchExpression
        <|> recordExpression
        <|> ifExpression
        <|> lambdaExpression
        <|> letExpression
        <|> variableExpression
        <|> parens expressionParser
    rest <- optional (symbol_ "." *> name)
    return $
      case rest of
        Just ll ->
          ESelect () (Label () ll) e1
        Nothing ->
          e1

functionCall :: Parser (Expression () ())
functionCall = do
  fn <- try (parens expressionParser) <|> dataConstructor <|> EVariable () . Label () <$> name
  as <- parens (commaSep expressionParser)
  pure $
    EApplication () () fn $
      case as of
        arg : args ->
          arg :| args
        [] ->
          ELiteral () LUnit :| []

dataConstructor :: Parser (Expression () ())
dataConstructor = EConstructor () . Label () <$> constructor

patternBinding :: Parser (Binding Expression () ())
patternBinding = BPattern () <$> (patternParser <* symbol "=") <*> expressionParser

functionBinding :: Parser (Binding Expression () ())
functionBinding = undefined

letBinding :: Parser (Binding Expression () ())
letBinding = patternBinding <|> functionBinding

letExpression :: Parser (Expression () ())
letExpression = do
  bs <- lexeme_ "let" *> semicolonSep1 letBinding
  e <- lexeme_ "in" *> expressionParser
  case bs of
    b : bs1 ->
      pure (ELet () (b :| bs1) e)
    _ ->
      error "Implementation error"

choice :: Parser (Choice Expression () ())
choice = CPlain () [] <$> expressionParser

clause :: Parser (Clause () ())
clause = do
  p <- symbol_ "|" *> patternParser
  c : cs <- symbol_ "=>" *> some choice
  pure (EClause () p (c :| cs))

foldExpression :: Parser (Expression () ())
foldExpression = do
  lexeme_ "fold"
  expr : exprs <- parens (commaSep1 expressionParser)
  c : cs <- braces (some clause)
  pure (EFold () () (expr :| exprs) (c :| cs) Nothing)

variableExpression :: Parser (Expression () ())
variableExpression = EVariable () . Label () <$> name

choiceParser = do
  -- TODO
  e <- expressionParser
  pure (CPlain () [] e)

matchClause :: Parser (Clause () ())
matchClause = do
  symbol_ "|"
  p <- patternParser
  symbol_ "=>"
  cs <- some choiceParser
  case cs of
    c : cs1 ->
      pure (EClause () p (c :| cs1))
    _ ->
      error "Implementation error"

matchExpression :: Parser (Expression () ())
matchExpression = do
  lexeme_ "match"
  e <- parens expressionParser
  cs <- braces (some matchClause)
  case cs of
    c : cs1 ->
      pure (EMatch () () e (c :| cs1))
    _ ->
      error "Implementation error"

ifExpression :: Parser (Expression () ())
ifExpression = do
  lexeme_ "if"
  e1 <- expressionParser
  lexeme_ "then"
  e2 <- expressionParser
  lexeme_ "else"
  e3 <- expressionParser
  pure (EIf () () e1 e2 e3)

lambdaExpression :: Parser (Expression () ())
lambdaExpression = do
  args <- lexeme_ "fn" *> parens (commaSep1 patternParser)
  e <- symbol_ "=>" *> expressionParser
  case args of
    a : as ->
      pure (ELambda () (a :| as) e)
    _ ->
      error "Implementation error"

recordExpression :: Parser (Expression () ())
recordExpression = do
  kvs <- braces (commaSep1 field)
  -- TODO
  pure (ERecord () () (Map.fromList kvs) Nothing)
 where
  field :: Parser (Name, Expression () ())
  field = (,) <$> name <*> (symbol_ "=" *> expressionParser)

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
literalExpression =
  listLiteral
    <|> lexeme_ "true" $> ELiteral () (LBool True)
    <|> lexeme_ "false" $> ELiteral () (LBool False)

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
  , -- TODO
    InfixR (binaryOperator OReverseApplication <$ symbol "|.")
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
fixity4 =
  [ InfixR (EListCons () () <$ symbol "::")
  ]
fixity3 =
  [ InfixR (binaryOperator OLogicalAnd <$ symbol "&&")
  ]
fixity2 =
  [ InfixN (binaryOperator OLessThan <$ symbol "<")
  , InfixN (binaryOperator OGreaterThan <$ symbol ">")
  , InfixR (binaryOperator OLogicalOr <$ symbol "||")
  ]

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
  , [Postfix (symbol_ ":" *> (EAnnotation () <$> typeParser))]
  ]
