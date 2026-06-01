{- | Surface language modules

This barrel re-exports the core language modules that form Coal's type system,
expression language, and program structure. It serves as a convenience for
internal compiler code.

Note: Module sub-namespaces (Module.Import, Module.Export, Module.Path) are
not re-exported - import them explicitly when needed.
-}
module Coal.Language (
  module Coal.Language.Data.Constructor,
  module Coal.Language.Type.Indexed,
  module Coal.Language.HasKind,
  module Coal.Language.HasType,
  module Coal.Language.HasActive,
  module Coal.Language.Type,
  module Coal.Language.Type.Operations,
  module Coal.Language.Type.Intrinsic,
  module Coal.Language.Type.Kind,
  module Coal.Language.Type.Scheme,
  module Coal.Language.Type.Row,
  module Coal.Language.Trait,
  module Coal.Language.Pattern,
  module Coal.Language.Primitive,
  module Coal.Language.Expression,
  module Coal.Language.Serializable,
  module Coal.Language.Expression.Choice,
  module Coal.Language.Expression.Binding,
  module Coal.Language.Expression.Operator,
  module Coal.Language.Definition,
  module Coal.Language.Module,
) where

import Coal.Language.Data.Constructor
import Coal.Language.Definition
import Coal.Language.Expression
import Coal.Language.Expression.Binding
import Coal.Language.Expression.Choice
import Coal.Language.Expression.Operator
import Coal.Language.HasActive
import Coal.Language.HasKind
import Coal.Language.HasType
import Coal.Language.Module
import Coal.Language.Pattern
import Coal.Language.Primitive
import Coal.Language.Serializable
import Coal.Language.Trait
import Coal.Language.Type
import Coal.Language.Type.Indexed
import Coal.Language.Type.Intrinsic
import Coal.Language.Type.Kind
import Coal.Language.Type.Operations
import Coal.Language.Type.Row
import Coal.Language.Type.Scheme
