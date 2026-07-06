{-# LANGUAGE OverloadedStrings #-}

{- |
Expression parser.

Parses Coal kernel language expressions using a precedence-climbing expression parser.

Handles:

  * Variables and constructors
  * Literals and primitives
  * Lambda abstractions (@fn@)
  * Let-bindings
  * Conditionals (@if@)
  * Pattern matching (@case@)
  * Operators (via operator table)
  * Record construction and extension
  * Function application
-}
module Coal.Kernel.Parser.Expr (
  expr,
  label,
) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (makeExprParser)
import qualified Data.List.NonEmpty as NonEmpty

import Text.Megaparsec ((<|>))

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Parser (
  Parser,
  backtickString,
  field,
  lexeme,
  parens,
  qualifiedConstructor,
  qualifiedName,
  reserved,
  spaces,
 )
import Coal.Kernel.Parser.Op (op)
import Coal.Kernel.Parser.Prim (prim)
import Coal.Kernel.Parser.Symbol (
  angleBrackets,
  arrow,
  at,
  braces,
  colon,
  commaSep1,
  emptyBraces,
  equals,
  pipe,
  semicolonSep1,
 )
import Coal.Kernel.Parser.Type (type_)
import Common (Name)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C

-- | Parse an expression
expr :: Parser (Expr Type)
expr = spaces *> pExpr

-- | Internal expression parser
pExpr :: Parser (Expr Type)
pExpr = makeExprParser pAtomExpr [[]]

-- | Parse an atomic expression
pAtomExpr :: Parser (Expr Type)
pAtomExpr =
  P.choice
    [ P.try pParenExpr
    , P.try pLit
    , P.try pLet
    , P.try pIf
    , P.try pLam
    , P.try pMatch
    , P.try pProj
    , P.try pCon
    , pApp
    , pRecord
    , pOp
    , pVar
    ]

-- | Parse a parenthesized expression
pParenExpr :: Parser (Expr Type)
pParenExpr = parens pExpr

-- | Parse a literal primitive value
pLit :: Parser (Expr Type)
pLit = ELit <$> prim

-- | Parse a label: name : type or `field-name` : type or Constructor : type
label :: Parser (Label Type)
label = do
  name <- pLabelName
  colon
  t <- type_
  return $ Label t name

{- | Parse a label name (identifier, constructor, or backtick string)
Supports qualified names like My.Utilities.find_min and $Cons
-}
pLabelName :: Parser Name
pLabelName =
  P.choice
    [ P.try backtickString
    , P.try qualifiedConstructor
    , qualifiedName C.letterChar <|> qualifiedName (C.char '_')
    ]

-- | Parse a variable: name : type
pVar :: Parser (Expr Type)
pVar = EVar <$> label

-- | Parse a constructor: Constructor : type
pCon :: Parser (Expr Type)
pCon = do
  name <- qualifiedConstructor
  colon
  t <- type_
  return $ ECon (Label t name)

-- | Parse a binding: label = expr
pBinding :: Parser (Binding Type)
pBinding = do
  lbl <- label
  equals
  e <- pExpr
  return $ Binding lbl e

-- | Parse let expression: let binding1; binding2; ... in body
pLet :: Parser (Expr Type)
pLet = do
  reserved "let"
  bindings <- semicolonSep1 pBinding
  reserved "in"
  body <- pExpr
  case NonEmpty.nonEmpty bindings of
    Just bs ->
      return $ ELet bs body
    Nothing ->
      fail "Let expression requires at least one binding"

-- | Parse if expression: if (cond) then trueExpr else falseExpr
pIf :: Parser (Expr Type)
pIf = do
  reserved "if"
  cond <- parens pExpr
  reserved "then"
  thenExpr <- pExpr
  reserved "else"
  elseExpr <- pExpr
  return $ EIf cond thenExpr elseExpr

-- | Parse lambda: fn(arg1 : type1, arg2 : type2, ...) => body
pLam :: Parser (Expr Type)
pLam = do
  reserved "fn"
  args <- parens (commaSep1 label)
  arrow
  body <- pExpr
  case NonEmpty.nonEmpty args of
    Just as ->
      return $ ELam as body
    Nothing ->
      fail "Lambda requires at least one argument"

-- | Parse function application: @<type>(expr1, expr2, ...)
pApp :: Parser (Expr Type)
pApp = do
  at
  t <- angleBrackets type_
  exprs <- parens (commaSep1 pExpr)
  case exprs of
    (e1 : e2 : es) -> case NonEmpty.nonEmpty (e2 : es) of
      Just args ->
        return $ EApp t e1 args
      Nothing ->
        fail "Application requires at least two expressions"
    _ ->
      fail "Application requires at least two expressions"

-- | Parse a clause: | (pattern1 : type1, pattern2 : type2, ...) => expr
pClause :: Parser (Clause Type)
pClause = do
  pipe
  patterns <- parens (commaSep1 label)
  arrow
  e <- pExpr
  case NonEmpty.nonEmpty patterns of
    Just ps ->
      return $ Clause ps e
    Nothing ->
      fail "Clause requires at least one pattern"

-- | Parse case expression: case<type>(scrutinee) { clause1 clause2 ... }
pMatch :: Parser (Expr Type)
pMatch = do
  reserved "case"
  t <- angleBrackets type_
  scrutinee <- parens pExpr
  clauses <- braces (P.some pClause)
  case NonEmpty.nonEmpty clauses of
    Just cs ->
      return $ ECase t scrutinee cs
    Nothing ->
      fail "Case requires at least one clause"

-- | Parse field projection: get?_fieldName<type>(expr)
pProj :: Parser (Expr Type)
pProj = do
  reserved "get"
  void $ lexeme (C.string "?_")
  fieldName <- field
  t <- angleBrackets type_
  rowExpr <- parens pExpr
  return $ EGet (Label t fieldName) rowExpr

-- | Parse record expression: {} or { field = value | rest }
pRecord :: Parser (Expr Type)
pRecord =
  P.choice
    [ P.try pEmptyRecord
    , pRecordExtension
    ]

-- | Parse empty record: {}
pEmptyRecord :: Parser (Expr Type)
pEmptyRecord = emptyBraces >> return ENil

-- | Parse record extension: { field = value | rest }
pRecordExtension :: Parser (Expr Type)
pRecordExtension = braces pRecordExt
 where
  pRecordExt = do
    fieldName <- field
    equals
    value <- pExpr
    pipe
    rest <- pRestRecord
    return $ EExt fieldName value rest

  pRestRecord =
    P.choice
      [ P.try pRecordExt -- Another field without braces
      , P.try pEmptyRecord -- Empty record {}
      , EVar <$> label -- Variable
      ]

-- | Parse operator expression
pOp :: Parser (Expr Type)
pOp = EOp <$> op pExpr
