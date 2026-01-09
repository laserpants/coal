{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Expression (parseExpression, parseMatchClause) where

import Coal.AST.HasMetadata (metadataSpan)
import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.Parser.Core
import Coal.Parser.Identifier (constructor, identifier, name)
import Coal.Parser.Metadata (withMetadata)
import Coal.Parser.Pattern (parsePattern, parseUnitPattern)
import Coal.Parser.Primitive (parsePrimitive)
import Coal.Parser.Symbol
import Coal.Parser.Type (parseType)
import Coal.Parser.Utils (fieldList)
import Control.Monad.Combinators.Expr
import qualified Data.ByteString.Char8 as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Extras (Name, isConstructor)
import GHC.Int (Int32, Int64)
import Text.Megaparsec (getSourcePos, notFollowedBy, option, optional, some, try, (<|>))
import Text.Megaparsec.Char (char, upperChar)
import qualified Text.Megaparsec.Char.Lexer as Lexer

parseAtom :: Parser (Expression Metadata ())
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

parseDoBlock :: Parser (Expression Metadata ())
parseDoBlock = do
  withMetadata $ do
    lexeme_ "do"
    exprs <- braces $ do
      nonEmpty $ some (try parseDoExpression <|> parseVoidDoExpression)
    pure (`EDoBlock` exprs)

parseDoExpression :: Parser (Pattern Metadata (), Expression Metadata ())
parseDoExpression = do
  p <- parsePattern
  symbol_ "<-"
  e <- parseExpression
  symbol_ ";"
  pure (p, e)

parseVoidDoExpression :: Parser (Pattern Metadata (), Expression Metadata ())
parseVoidDoExpression = do
  e <- parseExpression
  symbol_ ";"
  withMetadata $
    pure (\loc -> (PAny loc (), e))

parseSelectorOp :: Parser (Expression Metadata () -> Expression Metadata ())
parseSelectorOp = do
  start <- getSourcePos
  field <- try (symbol_ "." <* notFollowedBy (char '|')) *> (name <|> constructor)
  end <- getSourcePos
  pure (\expr -> selector expr (Metadata start end) field)

selectorPostfix :: Operator Parser (Expression Metadata ())
selectorPostfix = Postfix (foldl (flip (.)) id <$> some parseSelectorOp)

parseExpression :: Parser (Expression Metadata ())
parseExpression = makeExprParser parseAtom operator

selector :: Expression Metadata () -> Metadata -> Name -> Expression Metadata ()
selector expr loc lname
  | isConstructor lname = ECodataSelect loc ll (Just expr) Nothing
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
    f <-
      try (parens parseExpression)
        <|> parseSpecialNameExpression
        <|> parseVariableExpression
        <|> parseDataConstructor
    xs <- parens (nonEmptyOr parseUnit (commaSep parseExpression))
    pure (\loc -> EApplication loc () f xs)

parseFFICall :: Parser (Expression Metadata ())
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

parseDataConstructor :: Parser (Expression Metadata ())
parseDataConstructor =
  withMetadata $ do
    ll <- try parseQualifiedConstructor <|> parseSimpleConstructor
    pure (`EConstructor` ll)

parseSimpleConstructor :: Parser (Label ())
parseSimpleConstructor = Label () <$> constructor

parseQualifiedConstructor :: Parser (Label ())
parseQualifiedConstructor = do
  ns <- some (identifier upperChar <* symbol ".")
  n <- constructor
  pure (Label () (Text.intercalate "." ns <> "." <> n))

patternBinding :: Parser (Binding Expression Metadata ())
patternBinding =
  withMetadata $ do
    p <- parsePattern <* symbol "="
    e <- parseExpression
    pure (\loc -> BPattern loc p e)

functionBinding :: Parser (Binding Expression Metadata ())
functionBinding =
  withMetadata $ do
    n <- name
    ps <- parens (nonEmptyOr parseUnitPattern (commaSep parsePattern))
    symbol_ "="
    e <- parseExpression
    pure (\loc -> BFunction loc n ps e)

parseBinding :: Parser (Binding Expression Metadata ())
parseBinding = try functionBinding <|> patternBinding

parseLetExpression :: Parser (Expression Metadata ())
parseLetExpression =
  withMetadata $ do
    b <- lexeme_ "let" *> nonEmpty (semicolonSep1 parseBinding)
    e <- lexeme_ "in" *> parseExpression
    pure (\loc -> ELet loc b e)

parseChoice :: Parser (Choice Expression Metadata ())
parseChoice =
  withMetadata $ do
    gs <- option [] (parseOtherwise <|> parseGuard)
    symbol_ "=>"
    e <- parseExpression
    pure (\loc -> CPlain loc gs e)

parseOtherwise :: Parser [Guard Expression Metadata ()]
parseOtherwise = do
  withMetadata $ do
    lexeme_ "otherwise"
    pure (\loc -> [CGuard (ELiteral loc (LBool True))])

parseGuard :: Parser [Guard Expression Metadata ()]
parseGuard = do
  lexeme_ "when"
  g <- CGuard <$> parens parseExpression
  pure [g]

parseClause :: Parser (Clause Metadata ())
parseClause =
  withMetadata $ do
    p <- symbol_ "|" *> parsePattern
    cs <- nonEmpty (some parseChoice)
    pure (\loc -> EClause loc p cs)

parseFoldExpression :: Parser (Expression Metadata ())
parseFoldExpression = do
  withMetadata $ do
    lexeme_ "fold"
    es <- parens (nonEmpty (commaSep1 parseExpression))
    cs <- braces (nonEmpty (some parseClause))
    pure (\loc -> EFold loc () es cs)

parseSpecialNameExpression :: Parser (Expression Metadata ())
parseSpecialNameExpression =
  withMetadata $ do
    spec <-
      "nat$_pack"
        <|> "nat$_unpack"
        <|> "io$_println_string"
        <|> "io$_print_string"
        <|> "io$_println_int32"
        <|> "io$_print_int32"
        <|> "io$_println_int64"
        <|> "io$_print_int64"
        <|> "io$_println_bignum"
        <|> "io$_print_bignum"
        <|> "io$_println_bool"
        <|> "io$_print_bool"
        <|> "io$_println_char"
        <|> "io$_print_char"
        <|> "io$_println_float"
        <|> "io$_print_float"
        <|> "io$_println_double"
        <|> "io$_print_double"
        <|> "io$_eval"
        <|> "io$_return"
        <|> "string$_char_to_string"
        <|> "string$_bool_to_string"
        <|> "string$_int32_to_string"
        <|> "string$_float_to_string"
        <|> "string$_double_to_string"
        <|> "string$_to_list"
        <|> "string$_from_list"
        <|> "string$_reverse"
        <|> "string$_remove_whitespace"
        <|> "string$_tail"
        <|> "string$_length"
        <|> "string$_head_unsafe"
        <|> "number$_unsafe_parse_bignum"
        <|> "char$_ord"
        <|> "char$_chr"
    pure (\ll -> EVariable ll (Label () spec))

parseVariableExpression :: Parser (Expression Metadata ())
parseVariableExpression =
  withMetadata $ do
    ll <- try parseQualifiedName <|> (Label () <$> name)
    pure (`EVariable` ll)

parseQualifiedName :: Parser (Label ())
parseQualifiedName = do
  ns <- some (identifier upperChar <* symbol "." <* notFollowedBy (char '|'))
  n <- name
  pure (Label () (Text.intercalate "." ns <> "." <> n))

parseMatchClause :: Parser (Clause Metadata ())
parseMatchClause =
  withMetadata $ do
    p <- symbol_ "|" *> parsePattern
    cs <- nonEmpty (some parseChoice)
    pure (\loc -> EClause loc p cs)

parseLambdaMatchExpression :: Parser (Expression Metadata ())
parseLambdaMatchExpression = do
  withMetadata $ do
    lexeme_ "match"
    cs <- braces (nonEmpty (some parseMatchClause))
    pure (\loc -> ELambdaMatch loc () cs)

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
    pure (`fromLiteral` n)

fromLiteral :: Metadata -> Integer -> Expression Metadata ()
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

-- TODO: DRY
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

parseDivisionOperator :: Parser (Expression Metadata () -> Expression Metadata () -> Expression Metadata ())
parseDivisionOperator =
  withMetadata $
    pure
      ( \loc lhs rhs ->
          EApplication
            loc
            ()
            (EVariable loc (Label () "(/)"))
            (lhs :| [rhs])
      )

parseModulusOperator :: Parser (Expression Metadata () -> Expression Metadata () -> Expression Metadata ())
parseModulusOperator =
  withMetadata $
    pure
      ( \loc lhs rhs ->
          EApplication
            loc
            ()
            (EVariable loc (Label () "(%)"))
            (lhs :| [rhs])
      )

parseExponentiationOperator :: Parser (Expression Metadata () -> Expression Metadata () -> Expression Metadata ())
parseExponentiationOperator =
  withMetadata $
    pure
      ( \loc lhs rhs ->
          EApplication
            loc
            ()
            (EVariable loc (Label () "(^)"))
            (lhs :| [rhs])
      )

parseSemigroupOperator :: Parser (Expression Metadata () -> Expression Metadata () -> Expression Metadata ())
parseSemigroupOperator =
  withMetadata $
    pure
      ( \loc lhs rhs ->
          EApplication
            loc
            ()
            (EVariable loc (Label () "(<>)"))
            (lhs :| [rhs])
      )

parseEqualityOperator :: Parser (Expression Metadata () -> Expression Metadata () -> Expression Metadata ())
parseEqualityOperator =
  withMetadata $
    pure
      ( \loc lhs rhs ->
          EApplication
            loc
            ()
            (EVariable loc (Label () "(==)"))
            (lhs :| [rhs])
      )

parseInequalityOperator :: Parser (Expression Metadata () -> Expression Metadata () -> Expression Metadata ())
parseInequalityOperator =
  withMetadata $
    pure
      ( \loc lhs rhs ->
          EApplication
            loc
            ()
            (EVariable loc (Label () "(!=)"))
            (lhs :| [rhs])
      )

parseLessThanOrEqualOperator :: Parser (Expression Metadata () -> Expression Metadata () -> Expression Metadata ())
parseLessThanOrEqualOperator =
  withMetadata $
    pure
      ( \loc lhs rhs ->
          EApplication
            loc
            ()
            (EVariable loc (Label () "(<=)"))
            (lhs :| [rhs])
      )

parseGreaterThanOrEqualOperator :: Parser (Expression Metadata () -> Expression Metadata () -> Expression Metadata ())
parseGreaterThanOrEqualOperator =
  withMetadata $
    pure
      ( \loc lhs rhs ->
          EApplication
            loc
            ()
            (EVariable loc (Label () "(>=)"))
            (lhs :| [rhs])
      )

parseLessThanOperator :: Parser (Expression Metadata () -> Expression Metadata () -> Expression Metadata ())
parseLessThanOperator =
  withMetadata $
    pure
      ( \loc lhs rhs ->
          EApplication
            loc
            ()
            (EVariable loc (Label () "(<)"))
            (lhs :| [rhs])
      )

parseGreaterThanOperator :: Parser (Expression Metadata () -> Expression Metadata () -> Expression Metadata ())
parseGreaterThanOperator =
  withMetadata $
    pure
      ( \loc lhs rhs ->
          EApplication
            loc
            ()
            (EVariable loc (Label () "(>)"))
            (lhs :| [rhs])
      )

fixity9 :: [Operator Parser (Expression Metadata ())]
fixity9 =
  [ InfixR (binaryOperator OReverseComposition <$ symbol "<<")
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

fixity8, fixity7, fixity6, fixity5, fixity4, fixity3, fixity2, fixity1, fixity0 :: [Operator Parser (Expression Metadata ())]
fixity8 =
  [ Prefix negationOperator
  , Prefix (unaryOperator OLogicalNot <$ (symbol "!" <* notFollowedBy (char '=')))
  , InfixR (parseExponentiationOperator <* symbol "^")
  ]
fixity7 =
  [ InfixL (parseMultiplicationOperator <* symbol "*")
  , InfixL (parseDivisionOperator <* symbol "/")
  , InfixL (parseModulusOperator <* symbol "%")
  ]
fixity6 =
  [ InfixL (parseAdditionOperator <* try (symbol "+" <* notFollowedBy (char '+')))
  , InfixL (parseSubtractionOperator <* try (symbol "-"))
  , InfixR (parseSemigroupOperator <* symbol "<>")
  ]
fixity5 =
  [ InfixR (binaryOperator OListConcatenation <$ try (symbol "++" <* notFollowedBy (char '+')))
  , InfixR (binaryOperator OStringConcatenation <$ symbol "+++")
  , InfixR (listCons <$ symbol "::")
  ]
fixity4 =
  [ InfixN
      ( parseEqualityOperator
          <* symbol "=="
      )
  , InfixN
      ( parseInequalityOperator
          <* symbol "!="
      )
  , InfixN
      ( parseLessThanOrEqualOperator
          <* symbol "<="
      )
  , InfixN
      ( parseGreaterThanOrEqualOperator
          <* symbol ">="
      )
  , InfixN
      ( try
          ( parseLessThanOperator
              <* symbol "<"
              <* notFollowedBy (char '=' <|> char '<')
          )
      )
  , InfixN
      ( try
          ( parseGreaterThanOperator
              <* symbol ">"
              <* notFollowedBy (char '=' <|> char '>')
          )
      )
  ]
fixity3 =
  [ InfixR (binaryOperator OLogicalAnd <$ symbol "&&")
  ]
fixity2 =
  [ InfixR (binaryOperator OLogicalOr <$ symbol "||")
  ]
fixity1 =
  [ InfixL (binaryOperator OReverseApplication <$ symbol "|.")
  , InfixL (binaryOperator OForwardApplication <$ symbol ".|")
  ]
fixity0 =
  [ InfixR (binaryOperator OReverseComposition <$ symbol "<<")
  , InfixR (binaryOperator OForwardComposition <$ symbol ">>")
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
  , fixity1
  , fixity0
  , [Postfix annotation]
  ]
