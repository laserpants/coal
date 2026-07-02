{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

{- |
Top-level object declarations.

A Coal kernel language module consists of a sequence of top-level objects:

  * __Functions__: Named functions with parameters and a body expression
  * __Constants__: Named expressions evaluated at module initialization
  * __Externals__: Foreign function declarations with type signatures
  * __Data types__: Type declarations with a list of constructor names and types

The 'Object' type is parameterized over type annotations @t@, allowing it to
be used both before and after type checking.

For 'DData', the first 'Name' is the type name; the list contains constructor
names and their types, sorted lexicographically by constructor name.
-}
module Coal.Kernel.Language.Object (Object (..)) where

import Coal.Kernel.Language.Expr (Expr (..), Label (..))
import Common (Name)

data Object t
  = DFunction Name [Label t] (Expr t)
  | DConstant Name (Expr t)
  | DExternal Name t
  | DData Name [(Name, t)]
  deriving
    ( Show
    , Eq
    , Ord
    , Functor
    , Foldable
    , Traversable
    )
