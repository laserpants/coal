{-# LANGUAGE OverloadedStrings #-}

{- |
Operator parser.

Parses operator expressions in explicit syntax: @[op type?] (expr1, expr2)@
for binary operators and @[!] (expr)@ for unary operators.

Operators are parsed as constructors from the 'Op' type, each specialized to a
specific primitive type.
-}
module Coal.Kernel.Parser.Op (
  op,
) where

import Control.Monad (void)
import qualified Data.Text as T

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C

import Coal.Kernel.Language.Op (Op (..))
import Coal.Kernel.Parser (Parser, brackets, lexeme, pair, parens, word)

-- | Parse an operator expression
op :: Parser a -> Parser (Op a)
op p =
  P.choice
    [ P.try (pUnaryOp p)
    , pBinaryOp p
    ]

-- | Parse a binary operator: [op type?] (expr1, expr2)
pBinaryOp :: Parser a -> Parser (Op a)
pBinaryOp p = do
  ctor <- brackets pBinaryOpConstructor
  args <- parens (pair p p)
  return $ uncurry ctor args

-- | Parse a unary operator: [op type?] (expr)
pUnaryOp :: Parser a -> Parser (Op a)
pUnaryOp p = do
  ctor <- brackets pUnaryOpConstructor
  arg <- parens p
  return $ ctor arg

-- | Parse a binary operator constructor inside brackets
pBinaryOpConstructor :: Parser (a -> a -> Op a)
pBinaryOpConstructor = do
  sym <- pOp2Symbol
  mType <- P.optional word
  case mType of
    Just "int32" ->
      case sym of
        SymEq -> return OEqInt32
        SymNe -> return ONeInt32
        SymLt -> return OLtInt32
        SymGt -> return OGtInt32
        SymLte -> return OLteInt32
        SymGte -> return OGteInt32
        SymAdd -> return OAddInt32
        SymSub -> return OSubInt32
        SymMul -> return OMulInt32
        SymDiv -> return ODivInt32
        _ -> fail $ "Invalid operator '" ++ show sym ++ "' for type int32"
    Just "int64" ->
      case sym of
        SymEq -> return OEqInt64
        SymNe -> return ONeInt64
        SymLt -> return OLtInt64
        SymGt -> return OGtInt64
        SymLte -> return OLteInt64
        SymGte -> return OGteInt64
        SymAdd -> return OAddInt64
        SymSub -> return OSubInt64
        SymMul -> return OMulInt64
        SymDiv -> return ODivInt64
        _ -> fail $ "Invalid operator '" ++ show sym ++ "' for type int64"
    Just "float" ->
      case sym of
        SymEq -> return OEqFloat
        SymNe -> return ONeFloat
        SymLt -> return OLtFloat
        SymGt -> return OGtFloat
        SymLte -> return OLteFloat
        SymGte -> return OGteFloat
        SymAdd -> return OAddFloat
        SymSub -> return OSubFloat
        SymMul -> return OMulFloat
        SymDiv -> return ODivFloat
        _ -> fail $ "Invalid operator '" ++ show sym ++ "' for type float"
    Just "double" ->
      case sym of
        SymEq -> return OEqDouble
        SymNe -> return ONeDouble
        SymLt -> return OLtDouble
        SymGt -> return OGtDouble
        SymLte -> return OLteDouble
        SymGte -> return OGteDouble
        SymAdd -> return OAddDouble
        SymSub -> return OSubDouble
        SymMul -> return OMulDouble
        SymDiv -> return ODivDouble
        _ -> fail $ "Invalid operator '" ++ show sym ++ "' for type double"
    Just "bool" ->
      case sym of
        SymEq -> return OEqBool
        SymNe -> return ONeBool
        _ -> fail $ "Invalid operator '" ++ show sym ++ "' for type bool"
    Just "char" ->
      case sym of
        SymEq -> return OEqChar
        SymNe -> return ONeChar
        _ -> fail $ "Invalid operator '" ++ show sym ++ "' for type char"
    Nothing ->
      case sym of
        SymOr -> return OOr
        SymAnd -> return OAnd
        _ -> fail $ "Operator '" ++ show sym ++ "' requires a type annotation"
    Just t ->
      fail $ "Unknown type: " ++ T.unpack t

-- | Parse a unary operator constructor inside brackets
pUnaryOpConstructor :: Parser (a -> Op a)
pUnaryOpConstructor =
  P.choice
    [ P.try pNegOp
    , pNotOp
    ]

-- | Parse logical NOT: !
pNotOp :: Parser (a -> Op a)
pNotOp = do
  void $ lexeme (C.char '!')
  return ONot

-- | Parse negation: neg <type>
pNegOp :: Parser (a -> Op a)
pNegOp = do
  void $ lexeme (C.string "neg")
  t <- word
  case t of
    "int32" -> return ONegInt32
    "int64" -> return ONegInt64
    "float" -> return ONegFloat
    "double" -> return ONegDouble
    _ -> fail $ "Invalid type for negation: " ++ T.unpack t

-- | Binary operator symbols
data Op2Symbol
  = SymEq
  | SymNe
  | SymLt
  | SymGt
  | SymLte
  | SymGte
  | SymAdd
  | SymSub
  | SymMul
  | SymDiv
  | SymOr
  | SymAnd
  deriving (Show, Eq, Ord, Read)

-- | Parse a binary operator symbol
pOp2Symbol :: Parser Op2Symbol
pOp2Symbol =
  P.choice
    [ P.try (lexeme (C.string "==") >> return SymEq)
    , P.try (lexeme (C.string "!=") >> return SymNe)
    , P.try (lexeme (C.string "<=") >> return SymLte)
    , P.try (lexeme (C.string ">=") >> return SymGte)
    , lexeme (C.string "<") >> return SymLt
    , lexeme (C.string ">") >> return SymGt
    , lexeme (C.string "+") >> return SymAdd
    , lexeme (C.string "-") >> return SymSub
    , lexeme (C.string "*") >> return SymMul
    , lexeme (C.string "/") >> return SymDiv
    , P.try (lexeme (C.string "||") >> return SymOr)
    , lexeme (C.string "&&") >> return SymAnd
    ]
