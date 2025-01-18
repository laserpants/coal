{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.PatternMatching where

import Noll.Label (Label (..))
import Noll.Utils (Name)

data EnvelopeClause e t = EnvelopeClause (Label t) [Label t] (EnvelopeExpression e t)
  deriving (Show, Eq, Ord, Read)

data EnvelopePattern e t
  = MConstructor (Label t) [EnvelopePattern e t]
  | MVariable (Label t)
  | MLiteral t (e t)
  deriving (Show, Eq, Ord, Read)

--

data EnvelopeExpression e t
  = MFail
  | MExpression (e t)
  | MCase (Label t) [EnvelopeClause e t]
  | MConditional (Label t) (e t) (EnvelopeExpression e t) (EnvelopeExpression e t)
  deriving (Show, Eq, Ord, Read)

--

data PatternEquationBody e t = PatternEquationBody [EnvelopePattern e t] (EnvelopeExpression e t)
  deriving (Show, Eq, Ord, Read)

data HeadLiteralEquation e t = HeadLiteralEquation (e t) (PatternEquationBody e t)
  deriving (Show, Eq, Ord, Read)

data HeadVariableEquation e t = HeadVariableEquation (Label t) (PatternEquationBody e t)
  deriving (Show, Eq, Ord, Read)

data HeadConstructorEquation e t = HeadConstructorEquation (Label t) [EnvelopePattern e t] (PatternEquationBody e t)
  deriving (Show, Eq, Ord, Read)

{-# INLINE headConstructorName #-}
headConstructorName :: HeadConstructorEquation e t -> Name
headConstructorName (HeadConstructorEquation (Label _ name) _ _) = name

data PatternEquation e t
  = HeadLiteral (HeadLiteralEquation e t)
  | HeadVariable (HeadVariableEquation e t)
  | HeadConstructor (HeadConstructorEquation e t)
  | EmptyEquation (EnvelopeExpression e t)
  deriving (Show, Eq, Ord, Read)

