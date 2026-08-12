{-# LANGUAGE NamedFieldPuns #-}

{- |
Type checking for Coal kernel language modules.

Provides the top-level entry points for type checking parsed modules. The type
checker uses a constraint-based approach:

  1. Build a global environment from all module declarations
  2. Traverse each expression, emitting type errors for inconsistencies
  3. Accumulate all errors via a 'Writer' monad

Type checking is __permissive__: it continues after encountering errors to
report as many problems as possible in a single pass.

= Opaque types

The type @*@ (represented as 'TOpq') serves as a wildcard during checking. It
is compatible with any type in either direction, allowing polymorphic code to
pass through the checker.
-}
module Coal.Kernel.TypeCheck (
  checkModules,
  checkModulesFromFiles,
  printTypeCheckResults,
  prettyTypeError,
  TypeError (..),
  TypeErrorKind (..),
  Context (..),
) where

import Control.Monad (forM_)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (execWriter)
import Data.Either (lefts, rights)
import qualified Data.Text as Text

import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Text.Megaparsec (errorBundlePretty)

import Coal.Kernel.Eval.Load (parseModuleFile)
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Prettyprinter.Type (prettyType)
import Coal.Kernel.TypeCheck.Env (buildGlobalEnv)
import Coal.Kernel.TypeCheck.Error (Context (..), TypeError (..), TypeErrorKind (..))
import Coal.Kernel.TypeCheck.Module (checkModule)

{- | Check all modules for type consistency.

All modules must be supplied together so that cross-module references can be
resolved. Returns the list of type errors found; an empty list means the
programs are type-consistent.
-}
checkModules :: [Module Type] -> [TypeError]
checkModules modules =
  let env = buildGlobalEnv modules
   in execWriter (runReaderT (forM_ modules checkModule) env)

{- | Parse a list of @.corn@ files and type-check them together.

Returns @Left parseErrors@ if any file fails to parse, where each element is
the pretty-printed parse error for one file. Returns @Right typeErrors@ if all
files parse successfully; @typeErrors@ is empty when the program is
type-consistent. Each error is formatted via 'prettyTypeError'.
-}

-- | Format a 'TypeError' as a human-readable string.
prettyTypeError :: TypeError -> String
prettyTypeError TypeError{errorContext, errorKind} =
  prettyContext errorContext <> ":\n  " <> prettyErrorKind errorKind

prettyContext :: Context -> String
prettyContext (InModule name) = "In module " <> Text.unpack name
prettyContext (InObject name) = "In " <> Text.unpack name
prettyContext InExpression = "In expression"

prettyErrorKind :: TypeErrorKind -> String
prettyErrorKind (TypeMismatch expected actual) =
  "type mismatch"
    <> "\n    expected: "
    <> renderType expected
    <> "\n    got:      "
    <> renderType actual
prettyErrorKind (VariableNotFound name) =
  "variable not found: " <> Text.unpack name
prettyErrorKind (ConstructorNotFound name) =
  "constructor not found: " <> Text.unpack name
prettyErrorKind (ArityMismatch expected actual) =
  "arity mismatch: expected "
    <> show expected
    <> " argument(s), got "
    <> show actual
prettyErrorKind (FieldNotFound name typ) =
  "field not found: " <> Text.unpack name <> " in " <> renderType typ
prettyErrorKind (NotAFunction typ) =
  "not a function: " <> renderType typ
prettyErrorKind (ConditionNotBool typ) =
  "condition is not bool: " <> renderType typ
prettyErrorKind (BranchTypeMismatch t1 t2) =
  "branch type mismatch: " <> renderType t1 <> " vs " <> renderType t2

renderType :: Type -> String
renderType = Text.unpack . renderStrict . layoutPretty defaultLayoutOptions . prettyType

checkModulesFromFiles :: [FilePath] -> IO (Either [String] [String])
checkModulesFromFiles paths = do
  results <- traverse parseModuleFile paths
  let failures = lefts results
      modules = rights results
  if null failures
    then pure (Right (prettyTypeError <$> checkModules modules))
    else pure (Left (errorBundlePretty <$> failures))

{- | Parse, type-check, and print the results to stdout.

On parse failure, prints each parse error. On success, prints each type error
or a confirmation that the program is type-consistent.
-}
printTypeCheckResults :: [FilePath] -> IO ()
printTypeCheckResults paths = do
  result <- checkModulesFromFiles paths
  case result of
    Left parseErrors -> mapM_ putStrLn parseErrors
    Right [] -> putStrLn "No type errors found."
    Right typeErrors -> mapM_ putStrLn typeErrors
