{-# LANGUAGE OverloadedStrings #-}

{- |
Module parser.

Parses Coal kernel language module declarations, including:

  * Module headers with qualified names
  * Import lists
  * Top-level object declarations (functions, constants, data types, externals)

Entry point: 'module_'
-}
module Coal.Kernel.Parser.Module (
  module_,
) where

import Control.Monad (void)
import Data.List (sortBy)
import Data.Ord (comparing)

import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C

import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (FunctionScope (..), Object (..))
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Language.Type.HasType (foldType)
import Coal.Kernel.Parser (Parser, lexeme, qualifiedConstructor, qualifiedName, spaces)
import Coal.Kernel.Parser.Expr (expr, label)
import Coal.Kernel.Parser.Symbol (braces, commaSep1, equals, pipe)
import Coal.Kernel.Parser.Type (type_)
import Common (Name)

-- | Parse a module
module_ :: Parser (Module Type)
module_ = spaces *> pModule

-- | Internal module parser
pModule :: Parser (Module Type)
pModule = do
  void (lexeme (C.string "module"))
  name <- pName
  braces $ do
    imports <- P.many pImport
    objects <- P.some pObject
    return $
      Module
        { moduleName = name
        , moduleImports = imports
        , moduleObjects = objects
        }

-- | Parse a module or data name: last component must be uppercase
pName :: Parser Name
pName = qualifiedConstructor

-- | Parse an object name: starts with uppercase first component, last component may be any case
pObjectName :: Parser Name
pObjectName = qualifiedName C.upperChar <|> qualifiedName (C.char '_')

-- | Parse an import declaration: import Name
pImport :: Parser Name
pImport = do
  void (lexeme (C.string "import"))
  pObjectName

-- | Parse an object (data, constant, or function)
pObject :: Parser (Object Type)
pObject =
  P.choice
    [ P.try pData
    , P.try pConstant
    , pFunction
    ]

{- | Parse a grouped data declaration:

@
data TypeExpr
  = CtorName(field1, field2)
  | CtorName2
@

Constructors are sorted lexicographically and stored as `DData typeName [(ctorName, ctorType)]`.
The type name is extracted from the return type expression.
-}
pData :: Parser (Object Type)
pData = do
  void (lexeme (C.string "data"))
  retType <- type_
  equals
  ctors <- P.sepBy1 (pConstructor retType) pipe
  let sorted = sortBy (comparing fst) ctors
      typeName = extractTypeName retType
  return $ DData typeName sorted

pConstructor :: Type -> Parser (Name, Type)
pConstructor retType = do
  ctorName <- qualifiedConstructor
  mFields <- P.optional pFields
  let fieldTypes = maybe [] id mFields
      ctorType = foldType retType fieldTypes
  return (ctorName, ctorType)

pFields :: Parser [Type]
pFields = do
  void $ lexeme (C.char '(')
  fields <- P.sepBy1 type_ (lexeme (C.char ','))
  void $ lexeme (C.char ')')
  return fields

extractTypeName :: Type -> Name
extractTypeName (TCon name _) = name
extractTypeName t = error $ "Expected TCon as return type, got: " ++ show t

-- | Parse a constant: Name = expr
pConstant :: Parser (Object Type)
pConstant = do
  name <- pObjectName
  equals
  e <- expr
  return $ DConstant name e

-- | Parse a function: Name (param1, param2, ...) = expr
pFunction :: Parser (Object Type)
pFunction = do
  name <- pObjectName
  params <- parens (commaSep1 label)
  equals
  body <- expr
  return $ DFunction Exported name params body
 where
  parens :: Parser a -> Parser a
  parens p = do
    void $ lexeme (C.char '(')
    result <- p
    void $ lexeme (C.char ')')
    return result
