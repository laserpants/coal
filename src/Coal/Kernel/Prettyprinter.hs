{- |
Pretty printing for Coal kernel language syntax.

Provides human-readable rendering of expressions, types, modules, and other
syntactic elements. The pretty printer uses the @prettyprinter@ library and
supports configurable page widths.

= Rendering strategy

All rendering is width-aware: line breaks and indentation are inserted
automatically to respect the maximum column width. The default is 80 columns,
but this can be overridden with 'renderModuleWidth'.
-}
module Coal.Kernel.Prettyprinter (
  -- * Pretty printing functions
  prettyPrim,
  prettyOp,
  prettyType,
  prettyExpr,
  prettyLabel,
  prettyBinding,
  prettyClause,
  prettyObject,
  prettyModule,

  -- * Convenience rendering functions
  renderModule,
  renderModuleWidth,
) where

import Data.Text (Text)

import Prettyprinter (LayoutOptions (..), PageWidth (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

import Coal.Kernel.Language.Module (Module)
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Prettyprinter.Expr (prettyBinding, prettyClause, prettyExpr, prettyLabel)
import Coal.Kernel.Prettyprinter.Module (prettyModule)
import Coal.Kernel.Prettyprinter.Object (prettyObject)
import Coal.Kernel.Prettyprinter.Op (prettyOp)
import Coal.Kernel.Prettyprinter.Prim (prettyPrim)
import Coal.Kernel.Prettyprinter.Type (prettyType)

{- | Render a module to 'Text' with default layout options (80 column
width).
-}
renderModule :: Module Type -> Text
renderModule = renderModuleWidth 80

-- | Render a module to Text with custom page width
renderModuleWidth :: Int -> Module Type -> Text
renderModuleWidth width mod_ = renderStrict (layoutPretty opts doc)
 where
  opts = defaultLayoutOptions{layoutPageWidth = AvailablePerLine width 1.0}
  doc = prettyModule prettyType mod_
