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
(e.g. a domain-specific @IO<ExitCode>@) is accepted and yields exit status 0.

= What is and is not rejected

Only a type that is provably not an @IO@ action is rejected. In particular an
unresolved type variable is accepted: type inference leaves the entry point's
type unresolved for some programs (for example when the entry point reaches
its result through a forward reference), so rejecting variables would reject
well-behaved programs whose runtime value is in fact an @IO@ action. The
practical consequence is that a bare numeric literal is not caught, because an
unannotated literal is inferred as an unresolved type variable:

@
fun main() = 5        -- accepted (type is an unresolved variable)
fun main() = true     -- rejected (type is `bool`)
fun main() = foo()    -- accepted iff `foo` is inferred to return an IO action
@
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

{- | True when the entry point's type is provably /not/ an @IO@ action.

@IO<a>@ is accepted for any @a@: the element type only determines the process
exit status (see the module header). An unresolved type variable is accepted
too, because inference leaves the entry point's type unresolved for some
well-behaved programs. Only a type that can positively be identified as
something other than an @IO@ action is rejected.
-}
isRejectedEntryType :: IndexedType -> Bool
isRejectedEntryType = \case
  TApplication _ (TConstructor _ "IO") _ -> False
  TVariable _ -> False
  _ -> True
