{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Constraint.Generation.Error (ConstraintsGenError (..)) where

import Coal.TypeSystem.Constraint.Generation.Annotation.Error (TypeAnnotationError (..))
import Extras (Name)

data ConstraintsGenError a
  = ENoDataConstructor a Name
  | EDataConstructorArityMismatch a Name Int Int
  | EIllFormedTypeAnnotation (TypeAnnotationError a)
  | EFoldPatternInRegularMatch a
  deriving (Show, Eq, Ord, Read)
