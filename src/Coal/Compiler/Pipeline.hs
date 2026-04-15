{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pipeline (
  pipeline,
  compile,
  compileWithCFiles,
  prettyError,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Config (CompilerConfig (..))
import Coal.Compiler.Embedded (embedded)
import Coal.Compiler.Environment (emptyCompilerEnvironment)
import Coal.Compiler.Error (errorLocation)
import Coal.Compiler.Pass (Pass (..), liftPass, mapPass, tickBar, (>->))
import Coal.Compiler.Pass.LoweringPhase (loweringPhase)
import Coal.Compiler.Pass.LoweringPhase.Linking (passLinking)
import Coal.Compiler.Pass.PhaseParsing (phaseParsing)
import Coal.Compiler.Pass.PhasePreflight (phasePreflight)
import Coal.Compiler.Pass.TranslationPhase (translationPhasePasses)
import Coal.Compiler.Pass.TypePhase (typePhasePasses)
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Compiler.TypeInference.Errors (prettyErrorMessage)
import Coal.Language (Kind)
import Coal.Language.Module.Path (principalPath)
import Coal.Pretty (CoalPretty (..))
import Coal.TypeSystem.Constraint.Generation
import Coal.TypeSystem.Constraint.Generation.Stack
import Coal.TypeSystem.Kind.Error (KindError (..))
import Coal.TypeSystem.Substitution (normalizeTypeIndexes)
import Control.Monad (replicateM_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (MonadIO, forM_)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import System.Console.AsciiProgress
import Text.Megaparsec (errorBundlePretty)
import TextShow (showt)

pipeline :: (MonadIO m, MonadMask m) => Pass Metadata m [FilePath] ()
pipeline =
  phaseParsing
    >-> phasePreflight
    >-> mapPass (liftPass (typePhasePasses >-> translationPhasePasses))
    >-> loweringPhase
    >-> passLinking

extraTicks :: (MonadIO m) => [BuildEnvelope a] -> CompilerT Metadata m [BuildEnvelope a]
extraTicks units = do
  forM_ units $
    \case
      BCached{} -> replicateM_ 73 tickBar
      _ -> pure ()
  pure units

compileWithCFiles :: CompilerConfig -> [FilePath] -> [FilePath] -> IO ()
compileWithCFiles config files cFiles = do
  foo <-
    if configSilent config
      then do
        go2 Nothing
      else -- pure $ fromRight (error "???") z
      do
        displayConsoleRegions $ do
          pb <-
            newProgressBar
              def
                { pgTotal = (fromIntegral (length embedded + length files) * 73) + 28
                , pgWidth = 100
                , pgFormat = "Compiling [:bar] :current/:total"
                }
          go2 (Just pb)
  -- pure $ fromRight (error "???") z

  case foo of
    (e, CompilerState{..}, es) -> do
      forM_ (nub es) $
        \err -> do
          case errorLocation err of
            Just (ErrorLocation name _) ->
              putStrLn ("\nIn module '" <> Text.unpack name <> "':\n")
            Nothing ->
              pure ()
          Text.putStrLn (prettyError compilerSources err)
      case e of
        Left e1 ->
          print e1
        Right{} -> do
          pure ()
 where
  -- go progressBar = do
  --  evalCompilerT $
  --    runCompilerT (emptyCompilerEnvironment progressBar) $ do
  --      lift $ setConfigC config{configCFiles = configCFiles config <> cFiles}
  --      runPass pipeline files
  go2 progressBar = do
    runCompilerT (emptyCompilerEnvironment progressBar) $ do
      setConfigC config{configCFiles = configCFiles config <> cFiles}
      runPass pipeline files

compile :: CompilerConfig -> [FilePath] -> IO ()
compile config files = compileWithCFiles config files []

prettyRule :: (Show a) => InferenceRule Kind a -> Text
prettyRule =
  \case
    RuleAnnotation _ t1 _ ->
      "Type annotation doesn't match inferred type: " <> prettyType u1
     where
      u1 = normalizeTypeIndexes t1
    RuleLetImplicit _ _ t1 t2 ->
      "Cannot unify " <> prettyType u1 <> " with " <> prettyType u2 <> "."
     where
      u1 = normalizeTypeIndexes t1
      u2 = normalizeTypeIndexes t2
    RuleTypeConstraint _ name t1 s ->
      "Cannot unify the type of `" <> name <> "`\n\n  " <> prettyType s <> "\n\nwith type\n\n  " <> prettyType u1
     where
      u1 = normalizeTypeIndexes t1
    RuleTuple _ t1 t2 ->
      "Cannot unify tuple type " <> prettyType u1 <> " with " <> prettyType u2 <> "."
     where
      u1 = normalizeTypeIndexes t1
      u2 = normalizeTypeIndexes t2
    RuleListLiteral _ _ ->
      "List elements must all have the same type."
    RuleRecordEquality _ t1 t2 ->
      "Expected a record of the form " <> prettyType u1 <> ", but got " <> prettyType u2 <> "."
     where
      u1 = normalizeTypeIndexes t1
      u2 = normalizeTypeIndexes t2
    RuleAssumption _ t1 t2 ->
      "Cannot unify " <> prettyType u1 <> " with " <> prettyType u2 <> "."
     where
      u1 = normalizeTypeIndexes t1
      u2 = normalizeTypeIndexes t2
    RuleOrConstraint _ t1 t2 ->
      "Or-pattern left-hand side type " <> prettyType u1 <> " doesn't match right-hand side type " <> prettyType u2
     where
      u1 = normalizeTypeIndexes t1
      u2 = normalizeTypeIndexes t2
    e ->
      Text.pack ("TODO: " <> show e)

prettyType :: (CoalPretty t) => t -> Text
prettyType p = "`" <> (renderStrict . layoutPretty defaultLayoutOptions $ prettyCoal p) <> "`"

prettyConstraintsGenError :: (Show a) => ConstraintsGenError a -> Text
prettyConstraintsGenError =
  \case
    EIllFormedTypeAnnotation (EAnnotationNonDistinctParameter _ name) ->
      "Type annotation is too general: error in the parameter '" <> name <> "'."
    EFoldPatternInRegularMatch _ ->
      "Fold patterns are not supported in regular match expression clauses. Perhaps you intended to use a 'fold'?"
    ENoDataConstructor _ name ->
      "Data constructor '" <> name <> "' not in scope."
    e ->
      Text.pack ("TODO:" <> show e)

prettyKindInferenceError :: KindError -> Text
prettyKindInferenceError =
  \case
    ENoTypeConstructor name ->
      "No type constructor '" <> name <> "' in scope."
    ENoTrait name ->
      "No trait '" <> name <> "' in scope."
    ECannotUnifyKinds ->
      "Kind unification failed"
    EInfiniteKind ->
      "Infinite kind"

prettyError :: Environment Text -> CompilerError Metadata -> Text
prettyError env =
  \case
    ParserError file err ->
      "In file \"" <> Text.pack file <> "\":\n\n" <> Text.pack (errorBundlePretty err)
    BadModuleName file path ->
      "The module name '" <> path <> "' doesn't match the file name '" <> Text.pack file <> "'."
    BadFilename _ err ->
      Text.pack err
    NoModuleMain ->
      "No 'Main' module"
    ModuleCycle names ->
      "Module imports form a cycle: " <> showt names
    MisplacedImportStatement erl -> do
      errorMessage ["Misplaced import statement"] env erl
    ModuleNotFound name erl ->
      errorMessage ["No such module: " <> name] env erl
    SolverError rule erl ->
      errorMessage ["Type error: " <> prettyRule rule] env erl
    NameNotInScope name erl ->
      errorMessage ["Name not in scope: '" <> name <> "'"] env erl
    ConstraintsError e erl ->
      errorMessage ["Type error: " <> prettyConstraintsGenError e] env erl
    NonExhaustivePatterns erl ->
      errorMessage ["Non-exhaustive patterns"] env erl
    FoldPatternInRegularMatch erl ->
      errorMessage ["Fold pattern cannot appear in regular match expression"] env erl
    FoldPatternOutsideConstructor erl ->
      errorMessage ["Fold pattern cannot appear outside constructor"] env erl
    Shadowing name erl ->
      errorMessage ["Name shadowing: '" <> name <> "'"] env erl
    MissingInstance trait erl ->
      errorMessage ["Missing trait instance " <> prettyType trait] env erl
    NameAlreadyDefined name erl ->
      errorMessage ["Name already defined: '" <> name <> "'"] env erl
    ConflictingParameter name erl ->
      errorMessage ["Conflicting parameter name: '" <> name <> "'"] env erl
    TypeAliasCycle name erl ->
      errorMessage ["Cyclic definition in type alias: '" <> name <> "'"] env erl
    ImportNotInModule name path erl ->
      errorMessage ["The module '" <> principalPath path <> "' doesn't export '" <> name <> "'."] env erl
    ExportNotInModule name _ erl ->
      errorMessage ["A definition '" <> name <> "' doesn't exist in this module."] env erl
    MissingType name path erl ->
      errorMessage ["The module '" <> principalPath path <> "' doesn't export a type '" <> name <> "'."] env erl
    NoDataConstructorForType ctor name _ erl ->
      errorMessage ["No constructor '" <> ctor <> "' for type '" <> name <> "' in scope"] env erl
    TraitNotInScope trait erl ->
      errorMessage ["No trait '" <> trait <> "' in scope"] env erl
    MissingTraitDefinition name trait erl ->
      errorMessage ["A defintion for '" <> name <> "' is missing from the instance for trait '" <> trait <> "'"] env erl
    UnexpectedTraitDefinition name trait erl ->
      errorMessage ["The trait '" <> trait <> "' doesn't have a method '" <> name <> "'"] env erl
    MissingRequiredInstance name t erl ->
      errorMessage ["Missing required instance for trait '" <> name <> "<" <> prettyType t <> ">'"] env erl
    KindError err erl ->
      errorMessage ["Kind error: " <> prettyKindInferenceError err] env erl
    OrPatternVariableMismatch _ _ erl ->
      errorMessage ["Sub-patterns must bind the same variable in or-patterns"] env erl
    NamedFoldNotAllowed erl ->
      errorMessage ["Named fold pattern inside expression fold."] env erl

errorMessage :: [Text] -> Environment Text -> ErrorLocation Metadata -> Text
errorMessage msgs env (ErrorLocation path loc) =
  maybe
    (error "Implementation error")
    (prettyErrorMessage (("\n• " <>) <$> msgs) loc)
    (Environment.lookup path env)
