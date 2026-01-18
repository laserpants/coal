{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Error (
  ErrorLocation (..),
  CompilerError (..),
  CompilerFailureMode (..),
  errorLocation,
) where

import Coal.Language (IndexedType, Kind (..), Trait (..))
import Coal.Language.Module.Path (Path (..))
import Coal.Parser (ParserError)
import Coal.TypeSystem.Constraint.Generation.InferenceRule (InferenceRule)
import Coal.TypeSystem.Constraint.Generation.Stack (ConstraintsGenError)
import Coal.TypeSystem.Kind.Inference (KindInferenceError (..))
import Data.Set (Set)
import Extras (Name)

data ErrorLocation a = ErrorLocation Name a
  deriving (Show, Eq)

data CompilerError a
  = ParserError FilePath ParserError
  | BadModuleName FilePath Name
  | BadFilename FilePath String
  | NoModuleMain
  | ModuleCycle [Name]
  | MisplacedImportStatement (ErrorLocation a)
  | ModuleNotFound Name (ErrorLocation a)
  | NonExhaustivePatterns (ErrorLocation a)
  | ConstraintsError (ConstraintsGenError a) (ErrorLocation a)
  | SolverError (InferenceRule Kind a) (ErrorLocation a)
  | NameNotInScope Name (ErrorLocation a)
  | FoldPatternInRegularMatch (ErrorLocation a)
  | FoldPatternOutsideConstructor (ErrorLocation a)
  | NamedFoldNotAllowed (ErrorLocation a)
  | Shadowing Name (ErrorLocation a)
  | MissingInstance (Trait IndexedType) (ErrorLocation a)
  | NameAlreadyDefined Name (ErrorLocation a)
  | ConflictingParameter Name (ErrorLocation a)
  | TypeAliasCycle Name (ErrorLocation a)
  | ImportNotInModule Name Path (ErrorLocation a)
  | ExportNotInModule Name Path (ErrorLocation a)
  | MissingType Name Path (ErrorLocation a)
  | MissingCotype Name Path (ErrorLocation a)
  | NoDataConstructorForType Name Name Path (ErrorLocation a)
  | NoCodataAccessorForCotype Name Name Path (ErrorLocation a)
  | TraitNotInScope Name (ErrorLocation a)
  | MissingTraitDefinition Name Name (ErrorLocation a)
  | UnexpectedTraitDefinition Name Name (ErrorLocation a)
  | MissingRequiredInstance Name IndexedType (ErrorLocation a)
  | KindError KindInferenceError (ErrorLocation a)
  | OrPatternVariableMismatch (Set Name) (Set Name) (ErrorLocation a)
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

errorLocation :: CompilerError a -> Maybe (ErrorLocation a)
errorLocation =
  \case
    ParserError{} ->
      Nothing
    BadModuleName{} ->
      Nothing
    ModuleCycle _ ->
      Nothing
    NoModuleMain ->
      Nothing
    BadFilename{} ->
      Nothing
    MisplacedImportStatement erl ->
      Just erl
    ModuleNotFound _ erl ->
      Just erl
    NonExhaustivePatterns erl ->
      Just erl
    ConstraintsError _ erl ->
      Just erl
    SolverError _ erl ->
      Just erl
    NameNotInScope _ erl ->
      Just erl
    FoldPatternInRegularMatch erl ->
      Just erl
    FoldPatternOutsideConstructor erl ->
      Just erl
    Shadowing _ erl ->
      Just erl
    MissingInstance _ erl ->
      Just erl
    NameAlreadyDefined _ erl ->
      Just erl
    ConflictingParameter _ erl ->
      Just erl
    TypeAliasCycle _ erl ->
      Just erl
    ImportNotInModule _ _ erl ->
      Just erl
    ExportNotInModule _ _ erl ->
      Just erl
    MissingType _ _ erl ->
      Just erl
    MissingCotype _ _ erl ->
      Just erl
    NoDataConstructorForType _ _ _ erl ->
      Just erl
    NoCodataAccessorForCotype _ _ _ erl ->
      Just erl
    TraitNotInScope _ erl ->
      Just erl
    MissingTraitDefinition _ _ erl ->
      Just erl
    UnexpectedTraitDefinition _ _ erl ->
      Just erl
    MissingRequiredInstance _ _ erl ->
      Just erl
    KindError _ erl ->
      Just erl
    OrPatternVariableMismatch _ _ erl ->
      Just erl
    NamedFoldNotAllowed erl ->
      Just erl
