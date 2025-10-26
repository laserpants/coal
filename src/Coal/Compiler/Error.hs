{-# LANGUAGE StrictData #-}

module Coal.Compiler.Error (
  ErrorLocation (..),
  CompilerError (..),
  CompilerFailureMode (..),
) where

import Coal.Language (IndexedType, Kind (..), Trait (..))
import Coal.Parser (ParserError)
import Coal.TypeSystem.Constraint.Generation.InferenceRule
import Coal.TypeSystem.Constraint.Generation.Internal
import Extra (Name)

data ErrorLocation a = ErrorLocation Name a
  deriving (Show, Eq)

data CompilerError a
  = ParserError FilePath ParserError
  | MisplacedImportStatement (ErrorLocation a)
  | ModuleNotFound Name (ErrorLocation a)
  | NonExhaustivePatterns (ErrorLocation a)
  | ConstraintsError (ConstraintsGenError a) (ErrorLocation a)
  | SolverError (InferenceRule Kind a) (ErrorLocation a)
  | NameNotInScope Name (ErrorLocation a)
  | FoldPatternInRegularMatch (ErrorLocation a)
  | FoldPatternOutsideConstructor (ErrorLocation a)
  | Shadowing Name (ErrorLocation a)
  | MissingInstance (Trait IndexedType) (ErrorLocation a)
  | NameAlreadyDefined Name (ErrorLocation a)
  | ConflictingParameter Name (ErrorLocation a)
  deriving (Show, Eq)

data CompilerFailureMode
  = ParserFailure
  | PreflightFailure
  | NoSuchIdentifier
  | MissingMainEntryPoint
  | TraitError
  | PatternAnomaly
  | TypeError
  | CompilerError
  deriving (Show, Eq, Ord, Read)
