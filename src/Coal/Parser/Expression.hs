{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Expression (parseExpression, parseMatchClause) where

import Coal.Ast.Metadata (Metadata (..), metadataSpan)
import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.Parser
import Coal.Parser.Identifier
import Coal.Parser.Metadata
import Coal.Parser.Pattern (parsePattern)
import Coal.Parser.Primitive (parsePrimitive)
import Coal.Parser.Symbol
import Coal.Parser.Type (parseType)
import Coal.Parser.Utils (fieldList)
import Control.Monad.Combinators.Expr
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Extra (Name, isConstructor)
import Text.Megaparsec (getSourcePos, notFollowedBy, optional, some, try, (<|>))
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as Lexer

parseAtom :: Parser (Expression Metadata ())
parseAtom =
  try parseFunctionApplication
    <|> parseDataConstructor
    <|> parseLiteralExpression
    <|> parseFoldExpression
    <|> try parseLambdaMatchExpression
    <|> parseMatchExpression
    <|> parseRecordExpression
    <|> parseIfExpression
    <|> parseLambdaExpression
    <|> parseLetExpression
    <|> parseVariableExpression
    <|> try (parens parseExpression)
    <|> parseTupleExpression

parseSelectorOp :: Parser (Expression Metadata () -> Expression Metadata ())
parseSelectorOp = do
  start <- getSourcePos
  field <- symbol_ "." *> (name <|> constructor)
  end <- getSourcePos
  pure (\expr -> selector expr (Metadata start end) field)

selectorPostfix :: Operator Parser (Expression Metadata ())
selectorPostfix = Postfix (foldl (flip (.)) id <$> some parseSelectorOp)

parseExpression :: Parser (Expression Metadata ())
parseExpression = makeExprParser parseAtom operator

selector :: Expression Metadata () -> Metadata -> Name -> Expression Metadata ()
selector expr loc lname
  | isConstructor lname = ECodataSelect loc ll expr Nothing
  | otherwise = ESelect loc ll expr
 where
  ll = Label () lname

parseUnit :: Parser (NonEmpty (Expression Metadata ()))
parseUnit =
  withMetadata $ do
    pure (\loc -> ELiteral loc LUnit :| [])

parseFunctionApplication :: Parser (Expression Metadata ())
parseFunctionApplication =
  withMetadata $ do
    f <- try (parens parseExpression) <|> parseDataConstructor <|> parseVariableExpression
    xs <- parens (nonEmptyOr parseUnit (commaSep parseExpression))
    pure (\loc -> EApplication loc () f xs)

parseDataConstructor :: Parser (Expression Metadata ())
parseDataConstructor =
  withMetadata $ do
    ll <- Label () <$> constructor
    pure (`EConstructor` ll)

patternBinding :: Parser (Binding Expression Metadata ())
patternBinding =
  withMetadata $ do
    p <- parsePattern <* symbol "="
    e <- parseExpression
    pure (\loc -> BPattern loc p e)

parseBinding :: Parser (Binding Expression Metadata ())
parseBinding = patternBinding -- <|> functionBinding

parseLetExpression :: Parser (Expression Metadata ())
parseLetExpression =
  withMetadata $ do
    b <- lexeme_ "let" *> nonEmpty (semicolonSep1 parseBinding)
    e <- lexeme_ "in" *> parseExpression
    pure (\loc -> ELet loc b e)

parseChoice :: Parser (Choice Expression Metadata ())
parseChoice =
  withMetadata $ do
    e <- parseExpression
    pure (\loc -> CPlain loc [] e)

parseClause :: Parser (Clause Metadata ())
parseClause =
  withMetadata $ do
    p <- symbol_ "|" *> parsePattern
    symbol_ "=>"
    c <- parseChoice
    pure (\loc -> EClause loc p (NonEmpty.singleton c))

parseFoldExpression :: Parser (Expression Metadata ())
parseFoldExpression = do
  withMetadata $ do
    lexeme_ "fold"
    es <- parens (nonEmpty (commaSep1 parseExpression))
    cs <- braces (nonEmpty (some parseClause))
    pure (\loc -> EFold loc () es cs Nothing)

parseVariableExpression :: Parser (Expression Metadata ())
parseVariableExpression =
  withMetadata $ do
    ll <- Label () <$> name
    pure (`EVariable` ll)

parseMatchClause :: Parser (Clause Metadata ())
parseMatchClause =
  withMetadata $ do
    p <- symbol_ "|" *> parsePattern
    cs <- symbol_ "=>"
    c <- parseChoice
    pure (\loc -> EClause loc p (NonEmpty.singleton c))

parseLambdaMatchExpression :: Parser (Expression Metadata ())
parseLambdaMatchExpression = do
  withMetadata $ do
    lexeme_ "match"
    cs <- braces (nonEmpty (some parseMatchClause))
    pure (\loc -> ELambdaMatch loc () cs Nothing)

parseMatchExpression :: Parser (Expression Metadata ())
parseMatchExpression = do
  withMetadata $ do
    lexeme_ "match"
    e <- parens parseExpression
    cs <- braces (nonEmpty (some parseMatchClause))
    pure (\loc -> EMatch loc () e cs)

parseIfExpression :: Parser (Expression Metadata ())
parseIfExpression =
  withMetadata $ do
    e1 <- lexeme_ "if" *> parseExpression
    e2 <- lexeme_ "then" *> parseExpression
    e3 <- lexeme_ "else" *> parseExpression
    pure (\loc -> EIf loc () e1 e2 e3)

parseLambdaExpression :: Parser (Expression Metadata ())
parseLambdaExpression =
  withMetadata $ do
    ps <- lexeme_ "fn" *> parens (nonEmpty (commaSep1 parsePattern))
    e <- symbol_ "=>" *> parseExpression
    pure (\loc -> ELambda loc ps e)

parseRecordExpression :: Parser (Expression Metadata ())
parseRecordExpression = do
  withMetadata $ do
    braces $ do
      fields <- fieldList parseExpression "="
      tail_ <- optional rest
      pure (\loc -> ERecord loc () (Map.fromList fields) tail_)
 where
  rest = pipe >> parseExpression

parseTupleExpression :: Parser (Expression Metadata ())
parseTupleExpression = do
  withMetadata $ do
    parens $ do
      exprs <- commaSep parseExpression
      case exprs of
        [] -> pure (`ELiteral` LUnit)
        (e : es) -> pure (\loc -> ETuple loc () (e :| es))

parseInt :: Parser (Expression Metadata ())
parseInt = do
  withMetadata $ do
    n <- Lexer.signed spaces (lexeme Lexer.decimal)
    pure
      ( \loc ->
          EApplication
            loc
            ()
            (EVariable loc (Label () "from_int32"))
            (ELiteral loc (LInt32 n) :| [])
      )

parseListLiteral :: Parser (Expression Metadata ())
parseListLiteral =
  withMetadata $ do
    es <- brackets (commaSep parseExpression)
    pure (\loc -> EListLiteral loc () es)

parseLiteralExpression :: Parser (Expression Metadata ())
parseLiteralExpression = parseListLiteral <|> parsePrimitive <|> parseInt

unaryOperator :: UnaryOperator -> Expression Metadata () -> Expression Metadata ()
unaryOperator op e1 =
  EApplication meta () (EUnaryOperator meta () op) (e1 :| [])
 where
  meta = metadataSpan e1 e1

binaryOperator :: BinaryOperator -> Expression Metadata () -> Expression Metadata () -> Expression Metadata ()
binaryOperator op e1 e2 =
  EApplication meta () (EBinaryOperator meta () op) (e1 :| [e2])
 where
  meta = metadataSpan e1 e2

listCons :: Expression Metadata () -> Expression Metadata () -> Expression Metadata ()
listCons e1 e2 = EListCons (metadataSpan e1 e2) () e1 e2

parseAdditionOperator :: Parser (Expression Metadata () -> Expression Metadata () -> Expression Metadata ())
parseAdditionOperator =
  withMetadata $
    pure
      ( \loc lhs rhs ->
          EApplication
            loc
            ()
            (EVariable loc (Label () "(+)"))
            (lhs :| [rhs])
      )

parseSubtractionOperator :: Parser (Expression Metadata () -> Expression Metadata () -> Expression Metadata ())
parseSubtractionOperator =
  withMetadata $
    pure
      ( \loc lhs rhs ->
          EApplication
            loc
            ()
            (EVariable loc (Label () "(-)"))
            (lhs :| [rhs])
      )

parseMultiplicationOperator :: Parser (Expression Metadata () -> Expression Metadata () -> Expression Metadata ())
parseMultiplicationOperator =
  withMetadata $
    pure
      ( \loc lhs rhs ->
          EApplication
            loc
            ()
            (EVariable loc (Label () "(*)"))
            (lhs :| [rhs])
      )

fixity9 :: [Operator Parser (Expression Metadata ())]
fixity9 =
  [ InfixR (binaryOperator OReverseComposition <$ symbol "<<")
  , InfixR (binaryOperator OReverseApplication <$ symbol "|.")
  ]

negationOperator :: Parser (Expression Metadata () -> Expression Metadata ())
negationOperator =
  withMetadata $ do
    symbol_ "-"
    pure $
      \loc e ->
        EApplication
          loc
          ()
          (EVariable loc (Label () "negate"))
          (e :| [])

fixity8 :: [Operator Parser (Expression Metadata ())]
fixity8 =
  [ Prefix negationOperator
  , Prefix (unaryOperator OLogicalNot <$ symbol "!")
  ]

fixity7 :: [Operator Parser (Expression Metadata ())]
fixity7 =
  [ InfixL (parseMultiplicationOperator <* symbol "*")
  ]

fixity6, fixity5, fixity4, fixity3, fixity2 :: [Operator Parser (Expression Metadata ())]
fixity6 =
  [ InfixL (parseAdditionOperator <* try (symbol "+" <* notFollowedBy (char '+')))
  , InfixL (parseSubtractionOperator <* try (symbol "-"))
  ]
fixity5 =
  [ InfixR (binaryOperator OListConcatenation <$ try (symbol "++" <* notFollowedBy (char '+')))
  , InfixR (binaryOperator OStringConcatenation <$ symbol "+++")
  , InfixR (listCons <$ symbol "::")
  ]
fixity4 =
  [ InfixN (binaryOperator OEqualTo <$ symbol "==")
  , InfixN (binaryOperator OLessThanOrEqual <$ symbol "<=")
  , InfixN (binaryOperator OGreaterThanOrEqual <$ symbol ">=")
  , InfixN (binaryOperator OLessThan <$ (symbol "<" <* notFollowedBy (char '=')))
  , InfixN (binaryOperator OGreaterThan <$ (symbol ">" <* notFollowedBy (char '=')))
  ]
fixity3 =
  [ InfixR (binaryOperator OLogicalAnd <$ symbol "&&")
  ]
fixity2 =
  [ InfixR (binaryOperator OLogicalOr <$ symbol "||")
  ]

annotation :: Parser (Expression Metadata () -> Expression Metadata ())
annotation = do
  start <- getSourcePos
  symbol_ ":"
  t <- parseType
  end <- getSourcePos
  pure (EAnnotation (Metadata start end) t)

operator :: [[Operator Parser (Expression Metadata ())]]
operator =
  [ [selectorPostfix]
  , fixity9
  , fixity8
  , fixity7
  , fixity6
  , fixity5
  , fixity4
  , fixity3
  , fixity2
  , [Postfix annotation]
  ]
