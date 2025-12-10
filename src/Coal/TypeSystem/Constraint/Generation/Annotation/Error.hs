{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Constraint.Generation.Annotation.Error (TypeAnnotationError (..)) where

import Coal.Language (Kind (..), Type (..), TypeIndex (..))
import Extras (Name)

data TypeAnnotationError a
  = -- Kind mismatch
    EAnnotationKindMismatch a
  | -- | Type constructor is not in scope
    EAnnotationConstructor a Name
  | -- | Two or more named parameters refer to the same inferred type variable.
    -- E.g., the annotation reads something like (a -> b) -> c -> b, but the
    -- function is fn(f, x) => f(x), which would require 'a' and 'c' to be the
    -- same type. The type signature claims that the function is polymorphic
    -- with respect to any choice of variables a, b, and c.
    EAnnotationNonDistinctParameter a Name
  | -- | Type parameter resolves to a concrete type; e.g.,
    -- fn(x : a, y : int32) => x + y
    EAnnotationMonomorphicType a Name (Type TypeIndex Kind)
  deriving (Show, Eq, Ord, Read)
