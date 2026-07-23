{-# LANGUAGE OverloadedStrings #-}

{- |
Object declaration pretty printing.

Renders top-level object declarations (functions, constants, data types,
externals) with appropriate formatting for parameter lists and bodies.
-}
module Coal.Kernel.Prettyprinter.Object (
  prettyObject,
) where

import qualified Data.List.NonEmpty as NonEmpty
import Prettyprinter (Doc, Pretty (..), line, nest, parens, punctuate, vsep, (<+>))

import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Language.Type.HasType (returnTypeOf, unfoldType)
import Coal.Kernel.Prettyprinter.Expr (prettyExpr, prettyLabel, prettyLeadingCommaList)

{- | Pretty print a top-level object.

Specialized to work with Type annotations.
-}
prettyObject :: (Type -> Doc ann) -> Object Type -> Doc ann
prettyObject pt obj =
  case obj of
    DData _typeName ctors ->
      -- Grouped data declaration:
      -- data ReturnType
      --   = CtorName(field1, field2)
      --   | CtorName2
      case ctors of
        [] -> error "DData with empty constructor list"
        (firstCtor : restCtors) ->
          let retType = returnTypeOf (snd firstCtor)
              prettyConstructor (ctorName, ctorType) =
                let typeList = NonEmpty.toList (unfoldType ctorType)
                    fieldTypes = init typeList -- drop the return type
                 in if null fieldTypes
                      then pretty ctorName
                      else
                        pretty ctorName
                          <> parens
                            (mconcat $ punctuate ", " (map pt fieldTypes))
              firstCtorLine = "  = " <> prettyConstructor firstCtor
              restCtorLines = map (("  | " <>) . prettyConstructor) restCtors
           in vsep $ ("data " <> pt retType) : firstCtorLine : restCtorLines
    DFunction _ name params body ->
      case params of
        [param] ->
          -- Single param: Name(param) = body
          pretty name
            <> parens (prettyLabel pt param)
            <+> "="
              <> nest 2 (line <> prettyExpr pt body)
        _ ->
          -- Multi param: Name\n  ( param1\n  , param2\n  ) =\n    body
          pretty name
            <> nest
              2
              ( line
                  <> prettyLeadingCommaList (map (prettyLabel pt) params)
                  <+> "="
                    <> nest 2 (line <> prettyExpr pt body)
              )
    DConstant name expr ->
      -- Qualified.Name = expr
      pretty name <+> "=" <> nest 2 (line <> prettyExpr pt expr)
    DExternal name typ ->
      -- external Qualified.Name : type
      "external" <+> pretty name <+> ":" <+> pt typ
