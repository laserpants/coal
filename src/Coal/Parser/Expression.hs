{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Expression (parseExpression, parseMatchClause) where

import Coal.Common.Label (Label (..))
import Coal.Compiler.HasMetadata (metadataSpan)
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Language
import qualified Coal.Parser.BuiltinNames as Builtin
import Coal.Parser.Common (parseQualifiedConstructor, parseSimpleConstructor)
import Coal.Parser.Core
import Coal.Parser.Identifier (constructor, identifier, name)
import Coal.Parser.Metadata (withMetadata)
import qualified Coal.Parser.Operator as Op
import Coal.Parser.Pattern (parsePattern, parseUnitPattern)
import Coal.Parser.Primitive (parsePrimitive)
import Coal.Parser.Symbol
import Coal.Parser.Type (parseType)
import Coal.Parser.Utils (fieldList)
import qualified Control.Monad.Combinators.Expr as Combinators
import qualified Data.ByteString.Char8 as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Extras (Name)
import GHC.Int (Int32, Int64)
import Text.Megaparsec (getSourcePos, notFollowedBy, option, optional, satisfy, some, try, (<|>))
import Text.Megaparsec.Char (char, upperChar)
import qualified Text.Megaparsec.Char.Lexer as Lexer

parseAtom :: Parser (Expression Metadata () ())
parseAtom =
  try parseDoBlock
    <|> try parseFunctionApplication
    <|> parseFFICall
    <|> parseVariableExpression
    <|> parseDataConstructor
    <|> parseLiteralExpression
    <|> parseFoldExpression
    <|> try parseLambdaMatchExpression
    <|> parseMatchExpression
    <|> parseRecordExpression
    <|> parseIfExpression
    <|> parseLambdaExpression
    <|> parseLetExpression
    <|> try (parens parseExpression)
    <|> parseTupleExpression

parseDoBlock :: Parser (Expression Metadata () ())
parseDoBlock = do
  withMetadata $ do
    lexeme_ "do"
    exprs <- braces $ do
      nonEmpty $ some (try parseDoExpression <|> parseVoidDoExpression)
    pure (`EDoBlock` exprs)

parseDoExpression :: Parser (Pattern Metadata () (), Expression Metadata () ())
parseDoExpression = do
  p <- parsePattern
  symbol_ "<-"
  e <- parseExpression
  symbol_ ";"
  pure (p, e)

parseVoidDoExpression :: Parser (Pattern Metadata () (), Expression Metadata () ())
parseVoidDoExpression = do
  e <- parseExpression
  symbol_ ";"
  withMetadata $
    pure (\loc -> (PAny loc (), e))

parseSelectorOp :: Parser (Expression Metadata () () -> Expression Metadata () ())
parseSelectorOp = do
  start <- getSourcePos
  field <- try (symbol_ "." <* notFollowedBy (char '|')) *> (name <|> constructor)
  end <- getSourcePos
  pure (\expr -> selector expr (Metadata start end) field)

parseApplicationOp :: Parser (Expression Metadata () () -> Expression Metadata () ())
parseApplicationOp = do
  start <- getSourcePos
  xs <- parens (nonEmptyOr parseUnit (commaSep parseExpression))
  end <- getSourcePos
  pure (\expr -> EApplication (Metadata start end) () expr xs)

selectorPostfix :: Combinators.Operator Parser (Expression Metadata () ())
selectorPostfix = Combinators.Postfix (foldl (flip (.)) id <$> some (try parseSelectorOp <|> parseApplicationOp))

parseExpression :: Parser (Expression Metadata () ())
parseExpression = Combinators.makeExprParser parseAtom operator

selector :: Expression Metadata () () -> Metadata -> Name -> Expression Metadata () ()
selector expr loc lname = ESelect loc (Label () lname) expr

parseUnit :: Parser (NonEmpty (Expression Metadata () ()))
parseUnit =
  withMetadata $ do
    pure (\loc -> ELiteral loc LUnit :| [])

parseFunctionApplication :: Parser (Expression Metadata () ())
parseFunctionApplication =
  withMetadata $ do
    f <-
      try (parens parseExpression)
        <|> parseSpecialNameExpression
        <|> parseVariableExpression
        <|> parseDataConstructor
    xs <- parens (nonEmptyOr parseUnit (commaSep parseExpression))
    pure (\loc -> EApplication loc () f xs)

parseFFICall :: Parser (Expression Metadata () ())
parseFFICall =
  withMetadata $ do
    symbol_ "#"
    (n, t) <- braces $ do
      n <- name
      symbol_ ":"
      t <- parseType
      pure (n, t)
    args <- parens (commaSep parseExpression)
    cont <- parens parseExpression
    pure (\loc -> EFFICall loc () (Label t n) args cont)

parseDataConstructor :: Parser (Expression Metadata () ())
parseDataConstructor =
  withMetadata $ do
    ll <- try parseQualifiedConstructor <|> parseSimpleConstructor
    pure (`EConstructor` ll)

patternBinding :: Parser (Binding Expression Metadata () ())
patternBinding =
  withMetadata $ do
    p <- parsePattern <* symbol "="
    e <- parseExpression
    pure (\loc -> BPattern loc p e)

functionBinding :: Parser (Binding Expression Metadata () ())
functionBinding =
  withMetadata $ do
    n <- name
    ps <- parens (nonEmptyOr parseUnitPattern (commaSep parsePattern))
    symbol_ "="
    e <- parseExpression
    pure (\loc -> BFunction loc n ps e)

parseBinding :: Parser (Binding Expression Metadata () ())
parseBinding = try functionBinding <|> patternBinding

parseLetExpression :: Parser (Expression Metadata () ())
parseLetExpression =
  withMetadata $ do
    b <- lexeme_ "let" *> nonEmpty (semicolonSep1 parseBinding)
    e <- lexeme_ "in" *> parseExpression
    pure (\loc -> ELet loc b e)

parseChoice :: Parser (Choice Expression Metadata () ())
parseChoice =
  withMetadata $ do
    gs <- option [] (parseOtherwise <|> parseGuard)
    symbol_ "=>"
    e <- parseExpression
    pure (\loc -> CPlain loc gs e)

parseOtherwise :: Parser [Guard Expression Metadata () ()]
parseOtherwise = do
  withMetadata $ do
    lexeme_ "otherwise"
    pure (\loc -> [CGuard (ELiteral loc (LBool True))])

parseGuard :: Parser [Guard Expression Metadata () ()]
parseGuard = do
  lexeme_ "when"
  g <- CGuard <$> parens parseExpression
  pure [g]

parseClause :: Parser (Clause Metadata () ())
parseClause =
  withMetadata $ do
    p <- symbol_ "|" *> parsePattern
    cs <- nonEmpty (some parseChoice)
    pure (\loc -> EClause loc p cs)

parseFoldExpression :: Parser (Expression Metadata () ())
parseFoldExpression = do
  withMetadata $ do
    lexeme_ "fold"
    es <- parens (nonEmpty (commaSep1 parseExpression))
    cs <- braces (nonEmpty (some parseClause))
    pure (\loc -> EFold loc () es cs)

parseSpecialNameExpression :: Parser (Expression Metadata () ())
parseSpecialNameExpression =
  withMetadata $ do
    spec <- lexeme $ try $ do
      t <- Text.pack <$> some (satisfy (\c -> c /= ' ' && c /= '(' && c /= ')' && c /= ',' && c /= '{' && c /= '}'))
      if Builtin.isBuiltinName t
        then pure t
        else fail "not a builtin name"
    pure (\ll -> EVariable ll (Label () spec))

parseVariableExpression :: Parser (Expression Metadata () ())
parseVariableExpression =
  withMetadata $ do
    ll <- try parseQualifiedName <|> (Label () <$> name)
    pure (`EVariable` ll)

parseQualifiedName :: Parser (Label ())
parseQualifiedName = do
  ns <- some (identifier upperChar <* symbol "." <* notFollowedBy (char '|'))
  n <- name
  pure (Label () (Text.intercalate "." ns <> "." <> n))

parseMatchClause :: Parser (Clause Metadata () ())
parseMatchClause =
  withMetadata $ do
    p <- symbol_ "|" *> parsePattern
    cs <- nonEmpty (some parseChoice)
    pure (\loc -> EClause loc p cs)

parseLambdaMatchExpression :: Parser (Expression Metadata () ())
parseLambdaMatchExpression = do
  withMetadata $ do
    lexeme_ "match"
    cs <- braces (nonEmpty (some parseMatchClause))
    pure (\loc -> ELambdaMatch loc () cs)

parseMatchExpression :: Parser (Expression Metadata () ())
parseMatchExpression = do
  withMetadata $ do
    lexeme_ "match"
    e <- parens parseExpression
    cs <- braces (nonEmpty (some parseMatchClause))
    pure (\loc -> EMatch loc () e cs)

parseIfExpression :: Parser (Expression Metadata () ())
parseIfExpression =
  withMetadata $ do
    e1 <- lexeme_ "if" *> parseExpression
    e2 <- lexeme_ "then" *> parseExpression
    e3 <- lexeme_ "else" *> parseExpression
    pure (\loc -> EIf loc () e1 e2 e3)

parseLambdaExpression :: Parser (Expression Metadata () ())
parseLambdaExpression =
  withMetadata $ do
    ps <- lexeme_ "fn" *> parens (nonEmptyOr parseUnitPattern (commaSep parsePattern))
    e <- symbol_ "=>" *> parseExpression
    pure (\loc -> ELambda loc ps e)

parseRecordExpression :: Parser (Expression Metadata () ())
parseRecordExpression = do
  withMetadata $ do
    braces $ do
      fields <- fieldList parseExpression "="
      tail_ <- optional rest
      pure (\loc -> ERecord loc () (Map.fromList fields) tail_)
 where
  rest = pipe >> parseExpression

parseTupleExpression :: Parser (Expression Metadata () ())
parseTupleExpression = do
  withMetadata $ do
    parens $ do
      exprs <- commaSep parseExpression
      case exprs of
        [] -> pure (`ELiteral` LUnit)
        (e : es) -> pure (\loc -> ETuple loc () (e :| es))

parseInt :: Parser (Expression Metadata () ())
parseInt = do
  withMetadata $ do
    n <- Lexer.signed spaces (lexeme Lexer.decimal)
    pure (`fromLiteral` n)

fromLiteral :: Metadata -> Integer -> Expression Metadata () ()
fromLiteral loc n
  | n <= fromIntegral (maxBound :: Int32) =
      EApplication loc () (EVariable loc (Label () "from_int32")) (ELiteral loc (LInt32 (fromIntegral n)) :| [])
  | n <= fromIntegral (maxBound :: Int64) =
      EApplication loc () (EVariable loc (Label () "from_int64")) (ELiteral loc (LInt64 (fromIntegral n)) :| [])
  | otherwise =
      EApplication
        loc
        ()
        (EVariable loc (Label () "from_bignum"))
        ( EApplication
            mempty
            ()
            (EVariable mempty (Label () "number$_unsafe_parse_bignum"))
            (ELiteral mempty (LString (ByteString.pack $ show n)) :| [])
            :| []
        )

parseListLiteral :: Parser (Expression Metadata () ())
parseListLiteral =
  withMetadata $ do
    es <- brackets (commaSep parseExpression)
    pure (\loc -> EListLiteral loc () es)

parseLiteralExpression :: Parser (Expression Metadata () ())
parseLiteralExpression = parseListLiteral <|> parsePrimitive <|> parseInt

unaryOperator :: Operator -> Expression Metadata () () -> Expression Metadata () ()
unaryOperator op e1 =
  EApplication meta () (EOperator meta () op) (e1 :| [])
 where
  meta = metadataSpan e1 e1

binaryOperator :: Operator -> Expression Metadata () () -> Expression Metadata () () -> Expression Metadata () ()
binaryOperator op e1 e2 =
  EApplication meta () (EOperator meta () op) (e1 :| [e2])
 where
  meta = metadataSpan e1 e2

listCons :: Expression Metadata () () -> Expression Metadata () () -> Expression Metadata () ()
listCons e1 e2 = EListCons (metadataSpan e1 e2) () e1 e2

fixity9 :: [Combinators.Operator Parser (Expression Metadata () ())]
fixity9 =
  [ Combinators.InfixR (binaryOperator OReverseComposition <$ symbol "<<")
  ]

negationOperator :: Parser (Expression Metadata () () -> Expression Metadata () ())
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

fixity8, fixity7, fixity6, fixity5, fixity4, fixity3, fixity2, fixity1, fixity0 :: [Combinators.Operator Parser (Expression Metadata () ())]
fixity8 =
  [ Combinators.Prefix negationOperator
  , Combinators.Prefix (unaryOperator OLogicalNot <$ (symbol "!" <* notFollowedBy (char '=')))
  , Combinators.InfixR (Op.parseExponentiationOperator <* symbol "^")
  ]
fixity7 =
  [ Combinators.InfixL (Op.parseMultiplicationOperator <* symbol "*")
  , Combinators.InfixL (Op.parseDivisionOperator <* symbol "/")
  , Combinators.InfixL (Op.parseModulusOperator <* symbol "%")
  ]
fixity6 =
  [ Combinators.InfixL (Op.parseAdditionOperator <* try (symbol "+" <* notFollowedBy (char '+')))
  , Combinators.InfixL (Op.parseSubtractionOperator <* try (symbol "-"))
  , Combinators.InfixR (Op.parseSemigroupOperator <* symbol "<>")
  ]
fixity5 =
  [ Combinators.InfixR (binaryOperator OListConcatenation <$ try (symbol "++" <* notFollowedBy (char '+')))
  , Combinators.InfixR (binaryOperator OStringConcatenation <$ symbol "+++")
  , Combinators.InfixR (listCons <$ symbol "::")
  ]
fixity4 =
  [ Combinators.InfixN
      ( Op.parseEqualityOperator
          <* symbol "=="
      )
  , Combinators.InfixN
      ( Op.parseInequalityOperator
          <* symbol "!="
      )
  , Combinators.InfixN
      ( Op.parseLessThanOrEqualOperator
          <* symbol "<="
      )
  , Combinators.InfixN
      ( Op.parseGreaterThanOrEqualOperator
          <* symbol ">="
      )
  , Combinators.InfixN
      ( try
          ( Op.parseLessThanOperator
              <* symbol "<"
              <* notFollowedBy (char '=' <|> char '<')
          )
      )
  , Combinators.InfixN
      ( try
          ( Op.parseGreaterThanOperator
              <* symbol ">"
              <* notFollowedBy (char '=' <|> char '>')
          )
      )
  ]
fixity3 =
  [ Combinators.InfixR (binaryOperator OLogicalAnd <$ symbol "&&")
  ]
fixity2 =
  [ Combinators.InfixR (binaryOperator OLogicalOr <$ symbol "||")
  ]
fixity1 =
  [ Combinators.InfixL (binaryOperator OReverseApplication <$ symbol "|.")
  , Combinators.InfixL (binaryOperator OForwardApplication <$ symbol ".|")
  ]
fixity0 =
  [ Combinators.InfixR (binaryOperator OReverseComposition <$ symbol "<<")
  , Combinators.InfixR (binaryOperator OForwardComposition <$ symbol ">>")
  ]

annotation :: Parser (Expression Metadata () () -> Expression Metadata () ())
annotation = do
  start <- getSourcePos
  symbol_ ":"
  t <- parseType
  end <- getSourcePos
  pure (EAnnotation (Metadata start end) t)

operator :: [[Combinators.Operator Parser (Expression Metadata () ())]]
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
  , fixity1
  , fixity0
  , [Combinators.Postfix annotation]
  ]
