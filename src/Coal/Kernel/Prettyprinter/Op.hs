{-# LANGUAGE OverloadedStrings #-}

{- |
Operator pretty printing.

Renders operator expressions in their explicit syntax:
@[op type] (expr1, expr2)@ for binary operators and @[!] (expr)@ for unary
operators.
-}
module Coal.Kernel.Prettyprinter.Op (
  prettyOp,
) where

import Prettyprinter (Doc, align, hang, line, nest, (<+>))

import Coal.Kernel.Language.Op (Op (..))

{- | Pretty print an operator expression.

Takes a function to pretty print the contained expressions.
-}
prettyOp :: (a -> Doc ann) -> Op a -> Doc ann
prettyOp pe (OEqInt32 a b) = opBinary "==" "int32" a b pe
prettyOp pe (OEqInt64 a b) = opBinary "==" "int64" a b pe
prettyOp pe (OEqFloat a b) = opBinary "==" "float" a b pe
prettyOp pe (OEqDouble a b) = opBinary "==" "double" a b pe
prettyOp pe (OEqChar a b) = opBinary "==" "char" a b pe
prettyOp pe (OEqBool a b) = opBinary "==" "bool" a b pe
prettyOp pe (ONeInt32 a b) = opBinary "!=" "int32" a b pe
prettyOp pe (ONeInt64 a b) = opBinary "!=" "int64" a b pe
prettyOp pe (ONeFloat a b) = opBinary "!=" "float" a b pe
prettyOp pe (ONeDouble a b) = opBinary "!=" "double" a b pe
prettyOp pe (ONeChar a b) = opBinary "!=" "char" a b pe
prettyOp pe (ONeBool a b) = opBinary "!=" "bool" a b pe
prettyOp pe (OLtInt32 a b) = opBinary "<" "int32" a b pe
prettyOp pe (OLtInt64 a b) = opBinary "<" "int64" a b pe
prettyOp pe (OLtFloat a b) = opBinary "<" "float" a b pe
prettyOp pe (OLtDouble a b) = opBinary "<" "double" a b pe
prettyOp pe (OGtInt32 a b) = opBinary ">" "int32" a b pe
prettyOp pe (OGtInt64 a b) = opBinary ">" "int64" a b pe
prettyOp pe (OGtFloat a b) = opBinary ">" "float" a b pe
prettyOp pe (OGtDouble a b) = opBinary ">" "double" a b pe
prettyOp pe (OLteInt32 a b) = opBinary "<=" "int32" a b pe
prettyOp pe (OLteInt64 a b) = opBinary "<=" "int64" a b pe
prettyOp pe (OLteFloat a b) = opBinary "<=" "float" a b pe
prettyOp pe (OLteDouble a b) = opBinary "<=" "double" a b pe
prettyOp pe (OGteInt32 a b) = opBinary ">=" "int32" a b pe
prettyOp pe (OGteInt64 a b) = opBinary ">=" "int64" a b pe
prettyOp pe (OGteFloat a b) = opBinary ">=" "float" a b pe
prettyOp pe (OGteDouble a b) = opBinary ">=" "double" a b pe
prettyOp pe (OAddInt32 a b) = opBinary "+" "int32" a b pe
prettyOp pe (OAddInt64 a b) = opBinary "+" "int64" a b pe
prettyOp pe (OAddFloat a b) = opBinary "+" "float" a b pe
prettyOp pe (OAddDouble a b) = opBinary "+" "double" a b pe
prettyOp pe (OSubInt32 a b) = opBinary "-" "int32" a b pe
prettyOp pe (OSubInt64 a b) = opBinary "-" "int64" a b pe
prettyOp pe (OSubFloat a b) = opBinary "-" "float" a b pe
prettyOp pe (OSubDouble a b) = opBinary "-" "double" a b pe
prettyOp pe (OMulInt32 a b) = opBinary "*" "int32" a b pe
prettyOp pe (OMulInt64 a b) = opBinary "*" "int64" a b pe
prettyOp pe (OMulFloat a b) = opBinary "*" "float" a b pe
prettyOp pe (OMulDouble a b) = opBinary "*" "double" a b pe
prettyOp pe (ODivInt32 a b) = opBinary "/" "int32" a b pe
prettyOp pe (ODivInt64 a b) = opBinary "/" "int64" a b pe
prettyOp pe (ODivFloat a b) = opBinary "/" "float" a b pe
prettyOp pe (ODivDouble a b) = opBinary "/" "double" a b pe
prettyOp pe (OOr a b) = opBinaryNoType "||" a b pe
prettyOp pe (OAnd a b) = opBinaryNoType "&&" a b pe
prettyOp pe (ONot a) = opUnaryNoType "!" a pe
prettyOp pe (ONegInt32 a) = opUnary "-" "int32" a pe
prettyOp pe (ONegInt64 a) = opUnary "-" "int64" a pe
prettyOp pe (ONegFloat a) = opUnary "-" "float" a pe
prettyOp pe (ONegDouble a) = opUnary "-" "double" a pe

-- | Helper for binary operators: [op type]\n( arg1\n, arg2\n)
opBinary :: Doc ann -> Doc ann -> a -> a -> (a -> Doc ann) -> Doc ann
opBinary opSymbol typeName arg1 arg2 pe =
  "["
    <> opSymbol
    <> " "
    <> typeName
    <> "]"
    <> nest 2 (line <> prettyLeadingCommaList [pe arg1, pe arg2])

-- | Helper for binary operators without type annotation: [op]\n( arg1\n, arg2\n)
opBinaryNoType :: Doc ann -> a -> a -> (a -> Doc ann) -> Doc ann
opBinaryNoType opSymbol arg1 arg2 pe =
  "["
    <> opSymbol
    <> "]"
    <> nest 2 (line <> prettyLeadingCommaList [pe arg1, pe arg2])

-- | Helper for unary operators: [op type]\n( arg\n)
opUnary :: Doc ann -> Doc ann -> a -> (a -> Doc ann) -> Doc ann
opUnary opSymbol typeName arg pe =
  "["
    <> opSymbol
    <> " "
    <> typeName
    <> "]"
    <> nest 2 (line <> prettyLeadingCommaList [pe arg])

-- | Helper for unary operators without type annotation: [op]\n( arg\n)
opUnaryNoType :: Doc ann -> a -> (a -> Doc ann) -> Doc ann
opUnaryNoType opSymbol arg pe =
  "["
    <> opSymbol
    <> "]"
    <> nest 2 (line <> prettyLeadingCommaList [pe arg])

{- | Helper: render a list with leading commas (manual format style)
Renders as:
  ( first
  , second
  , third
  )
where commas align with the opening paren and multi-line items are properly indented
-}
prettyLeadingCommaList :: [Doc ann] -> Doc ann
prettyLeadingCommaList [] = "()"
prettyLeadingCommaList (x : xs) =
  align
    ( "("
        <+> nest 2 x
        <> mconcat ((\item -> line <> hang 2 ("," <+> item)) <$> xs)
        <> line
        <> ")"
    )
