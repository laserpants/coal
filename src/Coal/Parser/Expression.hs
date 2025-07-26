{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Expression (parseExpression) where

import Coal.Common.Label (Label (..))
import Coal.Common.List1 (NonEmpty (..))
import Coal.Language
import Coal.Parser
import Coal.Parser.Identifier
import Coal.Ast.Metadata (Metadata (..))
import Coal.Parser.Pattern (parsePattern)
import Coal.Parser.Symbol
import Coal.Parser.Type (parseType)
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Extra (Name)
import Text.Megaparsec (optional, some, try, (<|>))

import qualified Data.Map.Strict as Map
import qualified Text.Megaparsec.Char.Lexer as Lexer

parseExpression :: Parser (Expression Metadata ())
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
    pure (maybe e1 (\ll -> ESelect undefined (Label () ll) e1) rest)

parseFunctionApplication :: Parser (Expression Metadata ())
parseFunctionApplication =
  EApplication undefined ()
    <$> (try (parens parseExpression) <|> parseDataConstructor <|> parseVariableExpression)
    <*> parens (nonEmptyOr (pure $ ELiteral undefined LUnit :| []) (commaSep parseExpression))

parseDataConstructor :: Parser (Expression Metadata ())
parseDataConstructor = EConstructor undefined . Label () <$> constructor

patternBinding :: Parser (Binding Expression Metadata ())
patternBinding = BPattern undefined <$> (parsePattern <* symbol "=") <*> parseExpression

parseBinding :: Parser (Binding Expression Metadata ())
parseBinding = patternBinding -- <|> functionBinding

parseLetExpression :: Parser (Expression Metadata ())
parseLetExpression =
  ELet undefined
    <$> (lexeme_ "let" *> nonEmpty (semicolonSep1 parseBinding))
    <*> (lexeme_ "in" *> parseExpression)

parseChoice :: Parser (Choice Expression Metadata ())
parseChoice = CPlain undefined [] <$> parseExpression

parseClause :: Parser (Clause Metadata ())
parseClause =
  EClause undefined
    <$> (symbol_ "|" *> parsePattern)
    <*> (symbol_ "=>" *> nonEmpty (some parseChoice))

parseFoldExpression :: Parser (Expression Metadata ())
parseFoldExpression = do
  lexeme_ "fold"
  EFold undefined ()
    <$> parens (nonEmpty (commaSep1 parseExpression))
    <*> braces (nonEmpty (some parseClause))
    <*> pure Nothing

parseVariableExpression :: Parser (Expression Metadata ())
parseVariableExpression = EVariable undefined . Label () <$> name

parseMatchClause :: Parser (Clause Metadata ())
parseMatchClause =
  EClause undefined
    <$> (symbol_ "|" *> parsePattern)
    <*> (symbol_ "=>" *> nonEmpty (some parseChoice))

parseMatchExpression :: Parser (Expression Metadata ())
parseMatchExpression = do
  lexeme_ "match"
  EMatch undefined ()
    <$> parens parseExpression
    <*> braces (nonEmpty (some parseMatchClause))

parseIfExpression :: Parser (Expression Metadata ())
parseIfExpression =
  EIf undefined ()
    <$> (lexeme_ "if" *> parseExpression)
    <*> (lexeme_ "then" *> parseExpression)
    <*> (lexeme_ "else" *> parseExpression)

parseLambdaExpression :: Parser (Expression Metadata ())
parseLambdaExpression =
  ELambda undefined
    <$> (lexeme_ "fn" *> parens (nonEmpty (commaSep1 parsePattern)))
    <*> (symbol_ "=>" *> parseExpression)

parseRecordExpression :: Parser (Expression Metadata ())
parseRecordExpression = do
  fields <- braces (commaSep1 field)
  -- TODO
  pure (ERecord undefined () (Map.fromList fields) Nothing)
 where
  field :: Parser (Name, Expression Metadata ())
  field = (,) <$> name <*> (symbol_ "=" *> parseExpression)

parseInt :: Parser (Expression Metadata ())
parseInt = do
  n <- Lexer.signed spaces (lexeme Lexer.decimal)
  pure $
    EApplication
      undefined
      ()
      (EVariable undefined (Label () "from_int32"))
      (ELiteral undefined (LInt32 n) :| [])

parseListLiteral :: Parser (Expression Metadata ())
parseListLiteral = EListLiteral undefined () <$> brackets (commaSep parseExpression)

parseLiteralExpression :: Parser (Expression Metadata ())
parseLiteralExpression =
  parseListLiteral
    <|> (lexeme_ "true" $> ELiteral undefined (LBool True))
    <|> (lexeme_ "false" $> ELiteral undefined (LBool False))

unaryOperator :: UnaryOperator -> Expression Metadata () -> Expression Metadata ()
unaryOperator op e1 =
  EApplication
    undefined
    ()
    (EUnaryOperator undefined () op)
    (e1 :| [])

binaryOperator :: BinaryOperator -> Expression Metadata () -> Expression Metadata () -> Expression Metadata ()
binaryOperator op e1 e2 =
  EApplication
    undefined
    ()
    (EBinaryOperator undefined () op)
    (e1 :| [e2])

fixity9 :: [Operator Parser (Expression Metadata ())]
fixity9 =
  [ -- TODO
    InfixR (binaryOperator OReverseComposition <$ symbol "<<")
  , -- TODO
    InfixR (binaryOperator OReverseApplication <$ symbol "|.")
  ]

fixity8 :: [Operator Parser (Expression Metadata ())]
fixity8 = []

fixity7 :: [Operator Parser (Expression Metadata ())]
fixity7 =
  [ InfixL (binaryOperator OMultiplication <$ symbol "*")
  ]

fixity6, fixity5, fixity4, fixity3, fixity2 :: [Operator Parser (Expression Metadata ())]
fixity6 = []
fixity5 =
  [ InfixR (binaryOperator OListConcatenation <$ symbol "++")
  , InfixR (EListCons undefined () <$ symbol "::")
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

operator :: [[Operator Parser (Expression Metadata ())]]
operator =
  [ fixity9
  , fixity8
  , fixity7
  , fixity6
  , fixity5
  , fixity4
  , fixity3
  , fixity2
  , [Postfix (symbol_ ":" *> (EAnnotation undefined <$> parseType))]
  ]
