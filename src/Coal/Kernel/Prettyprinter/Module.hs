{-# LANGUAGE OverloadedStrings #-}

{- |
Module pretty printing.

Renders Coal kernel language modules with import lists and object declarations.
Formats module headers, import statements, and top-level objects with
appropriate spacing.
-}
module Coal.Kernel.Prettyprinter.Module (
  prettyModule,
) where

import Prettyprinter (Doc, Pretty (..), braces, line, vsep, (<+>))

import Coal.Common.Name (Name)
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (Object)
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Prettyprinter.Object (prettyObject)

{- | Pretty print a module.

Takes a function to pretty print the type parameter.
Specialized to work with Type annotations.
-}
prettyModule :: (Type -> Doc ann) -> Module Type -> Doc ann
prettyModule pt (Module name imports objects) =
  -- module ModuleName {
  --
  -- import Path1
  -- import Path2
  --
  -- object1
  --
  -- object2
  --
  -- }
  "module"
    <+> pretty name
    <+> braces
      ( line
          <> line
          <> (if null imports then mempty else prettyImports imports <> line <> line)
          <> prettyObjects pt objects
          <> line
      )

-- | Pretty print imports
prettyImports :: [Name] -> Doc ann
prettyImports = vsep . map (\imp -> "import" <+> pretty imp)

-- | Pretty print objects with blank lines between them
prettyObjects :: (Type -> Doc ann) -> [Object Type] -> Doc ann
prettyObjects _ [] = mempty
prettyObjects pt [obj] = prettyObject pt obj
prettyObjects pt (obj : objs) =
  prettyObject pt obj <> line <> line <> prettyObjects pt objs
