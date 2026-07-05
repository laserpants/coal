{-# LANGUAGE OverloadedStrings #-}

{- |
Expression pretty printing.

Renders Coal kernel language expressions as human-readable text with
automatic indentation and line breaking. Uses the @prettyprinter@
library's layout algorithm to produce nicely formatted output.

= Formatting strategy

  * Multi-parameter lambdas and let-bindings are vertically aligned
  * Function applications use hanging indentation
  * Case expressions format clauses with consistent indentation
  * Nested expressions respect the page width and indent appropriately
-}
module Coal.Kernel.Prettyprinter.Expr (
  prettyExpr,
  prettyLabel,
  prettyBinding,
  prettyClause,
  prettyLeadingCommaList,
) where

import Data.List (intersperse)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)

import Prettyprinter (Doc, Pretty (..), align, braces, hang, line, nest, parens, vcat, vsep, (<+>))

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Prettyprinter.Op (prettyOp)
import Coal.Kernel.Prettyprinter.Prim (prettyPrim)

{- | Pretty print an expression.

Takes a function to pretty print the type parameter.
-}
prettyExpr :: (t -> Doc ann) -> Expr t -> Doc ann
prettyExpr pt expr =
  case expr of
    EVar lbl -> prettyLabel pt lbl
    ECon lbl -> prettyLabel pt lbl
    ELit prim -> prettyPrim prim
    ELam params body ->
      "fn"
        <> parens (prettyCommaSep (NonEmpty.toList params) (prettyLabel pt))
        <+> "=>"
          <> nest 2 (line <> prettyExpr pt body)
    EApp retType func args ->
      "@<"
        <> pt retType
        <> ">"
        <> nest
          2
          ( line
              <> prettyLeadingCommaList
                (prettyExpr pt func : map (prettyExpr pt) (NonEmpty.toList args))
          )
    ELet bindings body ->
      -- let binding format:
      -- let
      --   name : type = expr
      --   ;
      --   name2 : type = expr
      --   in
      --     body
      "let"
        <> nest
          2
          ( line
              <> vsep (intersperse ";" (map (prettyBinding pt) (NonEmpty.toList bindings)))
              <> line
              <> "in"
              <> nest 2 (line <> prettyExpr pt body)
          )
    EIf cond thenExpr elseExpr ->
      "if"
        <> nest
          2
          ( line
              <> parens (prettyExpr pt cond)
              <> line
              <> "then"
              <> nest 2 (line <> prettyExpr pt thenExpr)
              <> line
              <> "else"
              <> nest 2 (line <> prettyExpr pt elseExpr)
          )
    EOp op -> prettyOp (prettyExpr pt) op
    ECase retType scrutinee clauses ->
      -- case<returnType>(scrutinee : type) {
      --   | pattern => expr
      --   | pattern => expr
      -- }
      "case<"
        <> pt retType
        <> ">"
        <> parens (prettyExpr pt scrutinee)
        <+> braces
          ( nest
              2
              ( line
                  <> vsep (map (prettyClause pt) (NonEmpty.toList clauses))
              )
              <> line
          )
    ENil -> "{}"
    EExt field value rest ->
      -- Flatten nested record extensions: { x = 1 | y = 2 | {} }
      let (allFields, finalRest) = collectExtFields (EExt field value rest)
          fieldDocs =
            map
              ( \(f, v) ->
                  pretty f
                    <+> "="
                      <> nest 2 (line <> prettyExpr pt v)
              )
              allFields
          allDocs = fieldDocs ++ [prettyExpr pt finalRest]
          -- Prefix all items except the first with "| "
          docsWithPipes = case allDocs of
            [] -> []
            (x : xs) -> x : map ("|" <+>) xs
       in "{ "
            <> vcat docsWithPipes
            <> line
            <> "}"
    EGet (Label t fieldName) rowExpr ->
      "get^"
        <> pretty fieldName
        <> "<"
        <> pt t
        <> ">"
        <> parens (prettyExpr pt rowExpr)

{- | Helper: collect all fields from nested EExt expressions
Returns (list of fields, final rest expression)
-}
collectExtFields :: Expr t -> ([(Text, Expr t)], Expr t)
collectExtFields (EExt field value rest) =
  let (restFields, finalRest) = collectExtFields rest
   in ((field, value) : restFields, finalRest)
collectExtFields other = ([], other)

-- | Pretty print a label (name : type)
prettyLabel :: (t -> Doc ann) -> Label t -> Doc ann
prettyLabel pt (Label typ name) = pretty name <+> ":" <+> pt typ

-- | Pretty print a binding (name : type = expr)
prettyBinding :: (t -> Doc ann) -> Binding t -> Doc ann
prettyBinding pt (Binding lbl expr) =
  prettyLabel pt lbl <+> "=" <> nest 2 (line <> prettyExpr pt expr)

-- | Pretty print a pattern matching clause
prettyClause :: (t -> Doc ann) -> Clause t -> Doc ann
prettyClause pt (Clause patterns expr) =
  "|"
    <+> prettyLeadingCommaList (map (prettyLabel pt) (NonEmpty.toList patterns))
    <+> "=>"
      <> nest 4 (line <> prettyExpr pt expr)

-- | Helper: comma-separated list
prettyCommaSep :: [a] -> (a -> Doc ann) -> Doc ann
prettyCommaSep [] _ = mempty
prettyCommaSep [x] f = f x
prettyCommaSep (x : xs) f = f x <> "," <+> prettyCommaSep xs f

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
          <> mconcat (map (\item -> line <> hang 2 ("," <+> item)) xs)
          <> line
          <> ")"
    )
