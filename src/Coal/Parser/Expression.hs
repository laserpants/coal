{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Expression (
  parseExpression,
  parseChoice,
  parseMatchClause,
) where

import Coal.Common.Label (Label (..))
import Coal.Compiler.HasMetadata (getMetadata, metadataSpan)
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Language
import qualified Coal.Parser.BuiltinNames as Builtin
import Coal.Parser.Common (parseQualifiedConstructor, parseSimpleConstructor)
import Coal.Parser.Core
import Coal.Parser.Identifier (identifier, name)
import Coal.Parser.Metadata (withMetadata)
import qualified Coal.Parser.Operator as Op
import Coal.Parser.Pattern (parsePattern, parseUnitPattern)
import Coal.Parser.Primitive (parsePrimitive)
import Coal.Parser.Symbol
import Coal.Parser.Type (parseType)
import Coal.Parser.Utils (fieldList)
import Control.Monad (void)
import qualified Control.Monad.Combinators.Expr as Combinators
import qualified Data.ByteString.Char8 as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Extras (Name)
import GHC.Int (Int32, Int64)
import Text.Megaparsec (getSourcePos, many, notFollowedBy, option, optional, satisfy, some, try, (<|>))
import Text.Megaparsec.Char (char, digitChar, upperChar)

parseAtom :: Parser (Expression Metadata () ())
parseAtom =
  try parseDoBlock
    <|> try parseFieldSection
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
    stmts <- braces $ nonEmpty $ some parseDoStatement
    pure (`EDoBlock` stmts)

{- | A single statement of a @do@ block.

Three forms are accepted, in this order:

@
pat <- expr;   -- monadic bind
pat = expr;    -- pure let binding (also @f(pat, ...) = expr;@)
expr;          -- bare expression statement
@

A bare expression statement is represented as a bind whose pattern is a
wildcard, so that the desugaring stays uniform. The first two alternatives
are wrapped in 'try' because the pattern / binding parsers may consume input
before the distinguishing token (@<-@ or @=@) is seen.
-}
parseDoStatement :: Parser (DoStatement Metadata () ())
parseDoStatement =
  try parseDoBindStatement
    <|> try parseDoLetStatement
    <|> parseDoVoidStatement

parseDoBindStatement :: Parser (DoStatement Metadata () ())
parseDoBindStatement = do
  p <- parsePattern
  symbol_ "<-"
  e <- parseExpression
  symbol_ ";"
  pure (DoBind p e)

parseDoLetStatement :: Parser (DoStatement Metadata () ())
parseDoLetStatement = do
  b <- parseBinding
  symbol_ ";"
  pure (DoLet b)

parseDoVoidStatement :: Parser (DoStatement Metadata () ())
parseDoVoidStatement = do
  e <- parseExpression
  symbol_ ";"
  withMetadata $
    pure (\loc -> DoBind (PAny loc ()) e)

{- | Parse a field section, @_.f@.

Desugars to a lambda abstracting over the section argument, so @_.f@ is
@fn($field) => $field.f@ and @map(_.name, xs)@ is
@map(fn($field) => $field.name, xs)@.

The body extends over the whole postfix chain, which keeps the rule easy to
state: everything after @_.@ up to the end of the operand belongs to the
section. So @_.a.b@ is @fn($field) => $field.a.b@ and @_.a(x)@ is
@fn($field) => $field.a(x)@. Applying a section to an argument therefore
needs parentheses or the pipeline operator: @(_.a)(x)@ or @x .| _.a@.

@_@ and @.@ must be adjacent and a field name must follow immediately. So
@_ .a@ is not a section at all: it is an ordinary selection on a variable
named @_@ (which fails to type-check, as @_@ can never be bound), while
@_.@, @_.(x)@ and @_.Name@ are parse errors.
-}
parseFieldSection :: Parser (Expression Metadata () ())
parseFieldSection = do
  start <- getSourcePos
  try (void (char '_') *> void (char '.'))
  field <- name
  mid <- getSourcePos
  ops <- many postfixOp
  end <- getSourcePos
  pure (fieldSection (Metadata start end) (Metadata start mid) field ops)

-- | Abstract over a field section's argument and apply the parsed operations.
fieldSection :: Metadata -> Metadata -> Name -> [Expression Metadata () () -> Expression Metadata () ()] -> Expression Metadata () ()
fieldSection loc firstLoc field ops =
  ELambda
    loc
    (PVariable loc (Label () binder) :| [])
    (foldl (\e f -> f e) (selector (EVariable loc (Label () binder)) firstLoc field) ops)
 where
  -- Unspellable in source, so the binder can neither capture nor be captured
  -- by user code. 'DetectShadowing' ignores compiler-generated names.
  binder = "$field"

{- | Parse a record-field selector @.f@.

The field must be a lowercase (or underscore-initial) name. An uppercase name
after a dot is reserved for a module-qualified name (@List.map@,
@Data.Map.Empty@), which the atom parsers consume before any selector is
considered. Uppercase /fields/ were only ever produced by the removed
@cotype@ / @unfold@ codata syntax, whose capitalized fields were selected with
@x.Foo@; that syntax no longer exists, so @x.Foo@ is a parse error.
-}
parseSelectorOp :: Parser (Expression Metadata () () -> Expression Metadata () ())
parseSelectorOp = do
  start <- getSourcePos
  field <- try (symbol_ "." <* notFollowedBy (char '|')) *> name
  end <- getSourcePos
  pure (\expr -> selector expr (Metadata start end) field)

parseApplicationOp :: Parser (Expression Metadata () () -> Expression Metadata () ())
parseApplicationOp = do
  start <- getSourcePos
  xs <- parens (nonEmptyOr parseUnit (commaSep parseExpression))
  end <- getSourcePos
  pure (\expr -> EApplication (Metadata start end) () expr xs)

parseRecordUpdateOp :: Parser (Expression Metadata () () -> Expression Metadata () ())
parseRecordUpdateOp = do
  start <- getSourcePos
  fields <- braces (fieldList parseExpression "=")
  end <- getSourcePos
  pure (\expr -> ERecordUpdate (metadataSpan (getMetadata expr) (Metadata start end)) () expr (Map.fromList fields))

{- | One postfix operation: a field selector, an application, or a record
update.

Shared by the ordinary postfix chain and by the body of a @_.@ field section
so that the two cannot drift apart.
-}
postfixOp :: Parser (Expression Metadata () () -> Expression Metadata () ())
postfixOp = try parseSelectorOp <|> parseApplicationOp <|> try parseRecordUpdateOp

selectorPostfix :: Combinators.Operator Parser (Expression Metadata () ())
selectorPostfix = Combinators.Postfix (foldl (flip (.)) id <$> some postfixOp)

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
    p <- pipe *> parsePattern
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
    n <- integer
    pure (`fromLiteral` n)

fromLiteral :: Metadata -> Integer -> Expression Metadata () ()
fromLiteral loc n
  | n >= 0 && m <= fromIntegral (maxBound :: Int32) =
      fromInt "from_int32" (LInt32 (fromIntegral n))
  | n >= 0 && m <= fromIntegral (maxBound :: Int64) =
      fromInt "from_int64" (LInt64 (fromIntegral n))
  | n >= 0 =
      fromBignum "from_integer" n
  | n >= fromIntegral (minBound :: Int32) =
      fromInt "from_negative_int32" (LInt32 (fromIntegral n))
  | n >= fromIntegral (minBound :: Int64) =
      fromInt "from_negative_int64" (LInt64 (fromIntegral n))
  | otherwise =
      fromBignum "from_negative_integer" n
 where
  m = abs n
  fromInt name_ lit =
    EApplication loc () (EVariable loc (Label () name_)) (ELiteral loc lit :| [])
  fromBignum name_ v =
    EApplication
      loc
      ()
      (EVariable loc (Label () name_))
      ( EApplication
          mempty
          ()
          (EVariable mempty (Label () "number$_unsafe_parse_integer"))
          (ELiteral mempty (LString (ByteString.pack $ show v)) :| [])
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
    -- A minus sign immediately followed by a digit starts a signed integer
    -- literal ('integer' handles the sign), so leave it to the atom parsers
    -- instead of desugaring it to 'negate'. Any other use of '-'
    -- ('- x', '- 5', '-f(x)') keeps the prefix-negation meaning.
    try $ void (char '-') *> notFollowedBy digitChar
    spaces
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
