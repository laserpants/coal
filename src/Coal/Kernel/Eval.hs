{- |
Interpreter for Coal kernel language programs.

Provides an in-memory evaluator for testing and prototyping. The interpreter:

  * Parses modules from text or files
  * Builds a global environment from all module declarations
  * Evaluates function applications using call-by-value semantics
  * Supports external function bindings for I/O and primitive operations

= Evaluation model

The evaluator uses an environment-based call-by-value reduction strategy.
Expressions are reduced to values (constructors, closures, literals) before
being passed to functions. Pattern matching is supported via runtime
inspection of constructor tags.

= External bindings

Functions declared as @external@ in the source must be bound to Haskell
implementations via 'ExternalEnv' before evaluation. See
"Coal.Kernel.Eval.External" for details.
-}
module Coal.Kernel.Eval (
  -- * Top-level evaluation API
  EvalResult,
  evalFunction,
  evalFunctionFromTexts,
  evalFunctionFromFiles,

  -- * Re-exported types for callers
  module Coal.Kernel.Eval.Value,
  module Coal.Kernel.Eval.State,
  module Coal.Kernel.Eval.External,
) where

import Data.Text (Text)

import Text.Megaparsec (errorBundlePretty)

import Coal.Common.Name (Name)
import Coal.Kernel.Eval.Expr (apply)
import Coal.Kernel.Eval.External
import Coal.Kernel.Eval.Link (buildGlobalEnv)
import Coal.Kernel.Eval.Load (parseModuleFile, parseModuleText)
import Coal.Kernel.Eval.State
import Coal.Kernel.Eval.Value
import Coal.Kernel.Language.Module (Module)
import Coal.Kernel.Language.Type (Type)

-- ---------------------------------------------------------------------------
-- Public result type
-- ---------------------------------------------------------------------------

type EvalResult = Either EvalError Value

-- ---------------------------------------------------------------------------
-- Primary entry points
-- ---------------------------------------------------------------------------

{- | Evaluate a fully-qualified function name with a list of arguments,
given a list of modules and an extern table.

Example:
@
  result <- evalFunction defaultExterns modules "Main.main" [VUnit]
@
-}
evalFunction ::
  ExternTable ->
  [Module Type] ->
  -- | Fully qualified function/constant name to call.
  Name ->
  -- | Arguments to pass (use @[VUnit]@ for @main(_ : *)@).
  [Value] ->
  IO EvalResult
evalFunction externTable modules fnName args = do
  envResult <- buildGlobalEnv externTable modules
  case envResult of
    Left err ->
      return (Left err)
    Right env -> do
      result <- runEvalM env (lookupVar fnName)
      case result of
        Left err -> return (Left err)
        Right fnVal -> runEvalM env (apply fnVal args)

{- | Parse module sources from (sourceName, text) pairs, then call
'evalFunction'.
-}
evalFunctionFromTexts ::
  ExternTable ->
  -- | (source name for error messages, module text) pairs.
  [(String, Text)] ->
  Name ->
  [Value] ->
  IO (Either String EvalResult)
evalFunctionFromTexts externTable sources fnName args = do
  case traverse (uncurry parseModuleText) sources of
    Left bundle ->
      return (Left (errorBundlePretty bundle))
    Right modules ->
      fmap Right (evalFunction externTable modules fnName args)

{- | Read and parse module files from a list of file paths, then call
'evalFunction'.
-}
evalFunctionFromFiles ::
  ExternTable ->
  [FilePath] ->
  Name ->
  [Value] ->
  IO (Either String EvalResult)
evalFunctionFromFiles externTable paths fnName args = do
  parseResults <- traverse parseModuleFile paths
  case sequence parseResults of
    Left bundle ->
      return (Left (errorBundlePretty bundle))
    Right modules ->
      fmap Right (evalFunction externTable modules fnName args)
