{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Expression (parseExpression) where

import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Lang.Common.List1 (NonEmpty (..))
import Lang.Label (Label (..))
import Lang.Utils (Name)
import Noll.Language
import Noll.Parser
import Noll.Parser.Identifier
import Noll.Parser.Pattern (parsePattern)
import Noll.Parser.Symbol
import Noll.Parser.Type (parseType)
import Text.Megaparsec (optional, some, try, (<|>))

import qualified Data.Map.Strict as Map
import qualified Text.Megaparsec.Char.Lexer as Lexer

parseExpression :: Parser (Expression () ())
parseExpression = makeExprParser go operator
 where
  go = do
    e1 <-
      try parseFunctionApplication
        <|> parseDataConstructor
        <|> parseInt
        <|> parseLiteralExpression
        <|> parseFoldExpression
        <|> parseMatchExpression
        <|> parseRecordExpression
        <|> parseIfExpression
        <|> parseLambdaExpression
        <|> parseLetExpression
        <|> parseVariableExpression
        <|> parens parseExpression
    rest <- optional (symbol_ "." *> name)
    pure $
      case rest of
        Just ll ->
          ESelect () (Label () ll) e1
        Nothing ->
          e1

parseFunctionApplication :: Parser (Expression () ())
parseFunctionApplication = do
  fn <- try (parens parseExpression) <|> parseDataConstructor <|> parseVariableExpression
  as <- parens (commaSep parseExpression)
  pure $
    EApplication () () fn $
      case as of
        arg : args ->
          arg :| args
        [] ->
          ELiteral () LUnit :| []

parseDataConstructor :: Parser (Expression () ())
parseDataConstructor = EConstructor () . Label () <$> constructor

patternBinding :: Parser (Binding Expression () ())
patternBinding = BPattern () <$> (parsePattern <* symbol "=") <*> parseExpression

parseBinding :: Parser (Binding Expression () ())
parseBinding = patternBinding -- <|> functionBinding

parseLetExpression :: Parser (Expression () ())
parseLetExpression = do
  bs <- lexeme_ "let" *> semicolonSep1 parseBinding
  e <- lexeme_ "in" *> parseExpression
  case bs of
    b : bs1 ->
      pure (ELet () (b :| bs1) e)
    _ ->
      error "Implementation error"

parseChoice :: Parser (Choice Expression () ())
parseChoice = CPlain () [] <$> parseExpression

parseClause :: Parser (Clause () ())
parseClause = do
  p <- symbol_ "|" *> parsePattern
  c : cs <- symbol_ "=>" *> some parseChoice
  pure (EClause () p (c :| cs))

parseFoldExpression :: Parser (Expression () ())
parseFoldExpression = do
  lexeme_ "fold"
  EFold () () 
    <$> parens (nonEmpty (commaSep1 parseExpression))
    <*> braces (nonEmpty (some parseClause))
    <*> pure Nothing

parseVariableExpression :: Parser (Expression () ())
parseVariableExpression = EVariable () . Label () <$> name

parseMatchClause :: Parser (Clause () ())
parseMatchClause =
  EClause () 
    <$> (symbol_ "|" *> parsePattern)
    <*> (symbol_ "=>" *> nonEmpty (some parseChoice))

parseMatchExpression :: Parser (Expression () ())
parseMatchExpression = do
  lexeme_ "match"
  EMatch () ()
    <$> parens parseExpression
    <*> braces (nonEmpty (some parseMatchClause))

parseIfExpression :: Parser (Expression () ())
parseIfExpression =
  EIf () ()
    <$> (lexeme_ "if" *> parseExpression)
    <*> (lexeme_ "then" *> parseExpression)
    <*> (lexeme_ "else" *> parseExpression)

parseLambdaExpression :: Parser (Expression () ())
parseLambdaExpression =
  ELambda ()
    <$> (lexeme_ "fn" *> parens (nonEmpty (commaSep1 parsePattern)))
    <*> (symbol_ "=>" *> parseExpression)

parseRecordExpression :: Parser (Expression () ())
parseRecordExpression = do
  fields <- braces (commaSep1 field)
  -- TODO
  pure (ERecord () () (Map.fromList fields) Nothing)
 where
  field :: Parser (Name, Expression () ())
  field = (,) <$> name <*> (symbol_ "=" *> parseExpression)

parseInt :: Parser (Expression () ())
parseInt = do
  n <- Lexer.signed spaces (lexeme Lexer.decimal)
  pure $
    EApplication
      ()
      ()
      (EVariable () (Label () "from_int32"))
      (ELiteral () (LInt32 n) :| [])

parseListLiteral :: Parser (Expression () ())
parseListLiteral = EListLiteral () () <$> brackets (commaSep parseExpression)

parseLiteralExpression :: Parser (Expression () ())
parseLiteralExpression =
  parseListLiteral
    <|> (lexeme_ "true" $> ELiteral () (LBool True))
    <|> (lexeme_ "false" $> ELiteral () (LBool False))

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
  []
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
  , [Postfix (symbol_ ":" *> (EAnnotation () <$> parseType))]
  ]
