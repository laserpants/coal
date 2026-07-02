{-# LANGUAGE OverloadedStrings #-}

{- |
Type parser.

Parses Coal kernel language type expressions, including:

  * Type constructors (@int32@, @list@, etc.)
  * Function types (@A -> B@)
  * Record types (extensible rows with @{@ and @|@)
  * Opaque wildcard type (@*@)

Type parsing uses operator precedence parsing for arrows, with record
extensions handled separately.
-}
module Coal.Kernel.Parser.Type (
  type_,
) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)

import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import TextShow (showt)

import Coal.Kernel.Language.Type (Type (..))
import qualified Coal.Kernel.Language.Type.Constructors as T
import Coal.Kernel.Parser (
  Parser,
  commaSep,
  commaSepN,
  field,
  lexeme,
  parens,
  qualifiedConstructor,
  qualifiedName,
  reserved,
  spaces,
 )
import Coal.Kernel.Parser.Symbol (braces, colon, emptyBraces, pipe, slash, star)

-- | Parse a type expression
type_ :: Parser Type
type_ = spaces *> pType

-- | Internal type parser (doesn't consume leading whitespace)
pType :: Parser Type
pType = makeExprParser pAtomType typeOperators

-- | Operator table for type expressions
typeOperators :: [[Operator Parser Type]]
typeOperators =
  [ [InfixR (slash >> return T.arrow)]
  ]

-- | Parse an atomic (non-function) type
pAtomType :: Parser Type
pAtomType =
  P.choice
    [ P.try pParenType
    , pOpaque
    , pPrimitiveType
    , pList
    , pTuple
    , pRecord
    , pRowType
    , pTypeCon
    ]

-- | Parse a parenthesized type
pParenType :: Parser Type
pParenType = parens pType

-- | Parse the opaque type: *
pOpaque :: Parser Type
pOpaque = star >> return T.opaque

-- | Parse primitive types
pPrimitiveType :: Parser Type
pPrimitiveType =
  P.choice
    [ reserved "bool" >> return T.bool
    , reserved "char" >> return T.char
    , reserved "double" >> return T.double
    , reserved "float" >> return T.float
    , reserved "int32" >> return T.int32
    , reserved "int64" >> return T.int64
    , reserved "bignum" >> return T.bignum
    , reserved "string" >> return T.string
    , reserved "unit" >> return T.unit
    ]

-- | Parse a list type: list(element-type)
pList :: Parser Type
pList = do
  void $ lexeme (C.string "list")
  t <- parens pType
  return $ TCon "list" [t]

{- | Parse a tuple type: tupleN(type1, type2, ..., typeN) where N >= 2
Matches "tuple" + digit(s) + parenthesized types (no spaces).
Valid forms: tuple2(...), tuple3(...), tuple4(...), etc.
-}
pTuple :: Parser Type
pTuple = do
  P.try $ do
    void $ lexeme (C.string "tuple")
    -- Peek ahead to ensure there's a digit
    void $ P.lookAhead C.digitChar
  n <- L.decimal
  lexeme (return ()) -- consume trailing whitespace after the digit
  if n >= 2
    then parens $ do
      types <- commaSepN n pType
      return $ tupleT types
    else fail "Tuple size must be at least 2"

tupleT :: [Type] -> Type
tupleT ts = let arity = showt (length ts) in TCon ("tuple" <> arity) ts

-- | Parse a record type: record(row)
pRecord :: Parser Type
pRecord = do
  void $ lexeme (C.string "record")
  r <- parens pRow
  return $ TCon "record" [r]

-- | Parse a standalone row type wrapped in braces
pRowType :: Parser Type
pRowType = pRow

{- | Parse a type constructor: Name or Name(type1, type2, ...)
Only uppercase, $-prefixed, or underscore-prefixed constructors are allowed.
Lowercase type names are reserved for primitives and special syntax.
-}
pTypeCon :: Parser Type
pTypeCon = do
  name <- P.try qualifiedConstructor <|> qualifiedName (C.char '_')
  args <- P.optional (parens (commaSep pType))
  case args of
    Nothing -> return $ TCon name []
    Just ts -> return $ TCon name ts

-- | Parse a row type
pRow :: Parser Type
pRow =
  P.choice
    [ P.try pEmptyRow
    , P.try pOpaqueRow
    , P.try pRowWithBraces
    ]

-- | Parse empty row: {}
pEmptyRow :: Parser Type
pEmptyRow = emptyBraces >> return RNil

-- | Parse opaque row: *
pOpaqueRow :: Parser Type
pOpaqueRow = star >> return T.opaque

-- | Parse row with braces: {field : type | rest}
pRowWithBraces :: Parser Type
pRowWithBraces = braces pRowContent

-- | Parse row content (inside braces): field : type | rest
pRowContent :: Parser Type
pRowContent = do
  fieldName <- field
  colon
  fieldType <- pType
  pipe
  rest <- pRowTail
  return $ RExt fieldName fieldType rest
 where
  pRowTail =
    P.choice
      [ P.try (emptyBraces >> return RNil)
      , P.try (star >> return T.opaque)
      , P.try pRowContent -- Recurse without expecting another '{'
      ]
