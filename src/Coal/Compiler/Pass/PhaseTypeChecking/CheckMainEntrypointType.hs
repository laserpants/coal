{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- | Module: Coal.Compiler.Pass.PhaseTypeChecking.CheckMainEntrypointType

Validate the type of the program entry point.

The entry point (by default @main@ in the @Main@ module) must produce an
@IO@ action. The generated C entry point runs the action returned by the entry
point, so a @main@ that returns anything else has no well-defined behaviour.

The /element/ type of the action only affects the process exit status:
@IO<int32>@ supplies it and @IO<unit>@ means zero. Any other element type
(e.g. a domain-specific @IO<ExitCode>@, or a type variable) is accepted and
yields exit status 0.

= What is and is not rejected

The entry point's type must be an application of @IO@; every other type is
rejected, including a type variable. Definitions are inferred callees-first
(see 'Coal.Compiler.Pass.PhaseTypeChecking.TypeInference.orderDefinitions'), so
by the time this pass runs the entry point's type is fully resolved and a
variable here means the program genuinely does not produce an @IO@ action:

@
fun main() = 5             -- rejected: an integer literal is a trait-constrained
                           -- conversion (`from_int32`), whose result type
                           -- nothing pins down, so this is not an IO action
fun main() = true          -- rejected: type is `bool`
fun main() = foo()         -- rejected iff `foo` does not return an IO action
fun main() = IO.return(5)  -- accepted: IO<a> is accepted for any `a`
@

Only the head constructor is required to be @IO@; the element type is
unconstrained because it merely determines the process exit status (see
'Coal.Kernel.LLVM.Codegen.exitCodeOf').
-}
module Coal.Compiler.Pass.PhaseTypeChecking.CheckMainEntrypointType (
  passCheckMainEntrypointType,
) where

import Coal.Compiler.Config (configEntryPoint)
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Metadata (Metadata)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerError (..), CompilerFailureMode (..), CompilerT, ErrorLocation (..))
import Coal.Compiler.State (CompilerState (..))
import Coal.Language.Definition (Definition (..), FunctionDefinition (..), LetDefinition (..))
import Coal.Language.HasType (HasType (typeOf))
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (Path (..), principalPath)
import Coal.Language.Type (IndexedType, Type (..))
import Coal.Language.Type.Kind (Kind)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (get)
import Data.Maybe (listToMaybe, mapMaybe)
import Extras (Name)

{- | Entry point type check.

Reject an entry point whose type is provably not an @IO@ action.
-}
passCheckMainEntrypointType :: (MonadIO m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passCheckMainEntrypointType = Pass{runPass = passImpl}

passImpl :: (Monad m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
passImpl m@Module{modulePath, moduleDefinitions} = do
  CompilerState{compilerConfig} <- get
  let (entryModule, entryFunction) = case configEntryPoint compilerConfig of
        Nothing -> (Path ["Main"], "main")
        Just (modName, funcName) -> (Path [modName], funcName)
  if modulePath == entryModule
    then case entryFunctionType entryFunction moduleDefinitions of
      Just (loc, t)
        | isRejectedEntryType t -> do
            tellErrors [InvalidMainFunctionType entryFunction t (ErrorLocation (principalPath modulePath) loc)]
            throwError TypeError
      _ -> pure ()
    else pure ()
  return m

{- | The declared result type of the entry point function, together with the
source location of its definition. 'Nothing' when the module does not define a
function of that name; the missing entry point is reported separately by
'Coal.Compiler.Pass.PhasePreflight.DetectMainEntrypointMissing'.
-}
entryFunctionType :: Name -> [Definition Metadata Kind IndexedType] -> Maybe (Metadata, IndexedType)
entryFunctionType name = listToMaybe . mapMaybe entry
 where
  entry = \case
    DFunction loc n FunctionDefinition{functionDefinitionExpression}
      | n == name ->
          Just (loc, typeOf functionDefinitionExpression)
    DLet loc n LetDefinition{letDefinitionExpression}
      | n == name ->
          Just (loc, typeOf letDefinitionExpression)
    _ ->
      Nothing

{- | True when the entry point's type is not an @IO@ action.

@IO<a>@ is accepted for any @a@: the element type only determines the process
exit status (see the module header). Everything else is rejected, including a
type variable: inference resolves the entry point's type completely, so a
variable means the entry point does not produce an @IO@ action. The usual cause
is a bare numeric literal, whose type is a trait-constrained conversion that
nothing pins down.
-}
isRejectedEntryType :: IndexedType -> Bool
isRejectedEntryType = \case
  TApplication _ (TConstructor _ "IO") _ -> False
  _ -> True
