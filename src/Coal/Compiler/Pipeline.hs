{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Pipeline
Description: Main compiler pipeline orchestrating all compilation phases

This module defines the central compiler pipeline that coordinates all
compilation phases from source files to executable binaries. The pipeline
consists of six sequential phases: parsing, preflight, type checking,
translation, lowering, and linking. It also provides error formatting
utilities for presenting compilation errors to users.
-}
module Coal.Compiler.Pipeline (
  pipeline,
  compile,
  compileWithCFiles,
  prettyError,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Config (CompilerConfig (..))
import Coal.Compiler.Environment (emptyCompilerEnvironment)
import Coal.Compiler.Error (errorLocation)
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), liftPass, mapPass, (>->))
import qualified Coal.Compiler.Pass.Counts as Counts
import Coal.Compiler.Pass.PhaseLowering.KernelCodegen (passKernelCodegen)
import Coal.Compiler.Pass.PhaseLowering.KernelTranslate (passKernelTranslate)
import Coal.Compiler.Pass.PhaseLowering.Linking (passLinking)
import Coal.Compiler.Pass.PhaseParsing (phaseParsing)
import Coal.Compiler.Pass.PhasePreflight (phasePreflight)
import Coal.Compiler.Pass.PhaseTranslation (phaseTranslation)
import Coal.Compiler.Pass.PhaseTypeChecking (phaseTypeChecking)
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
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (modifyIORef', newIORef)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Extras (forM_)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import System.IO (hPutStr, stderr)
import Text.Megaparsec (errorBundlePretty)
import TextShow (showt)

import Coal.Compiler.Progress (ProgressRef, writeStatus, writeStatusSimple)

pipeline :: (MonadIO m) => Pass Metadata m [FilePath] ()
pipeline =
  timed "Parsing" phaseParsing
    >-> timed "Preflight" phasePreflight
    >-> phaseMainPasses (phaseTypeChecking >-> phaseTranslation)
    >-> timed "Kernel translate" (mapPass passKernelTranslate)
    >-> timed "Kernel codegen" passKernelCodegen
    >-> timed "Linking" passLinking

timed :: (MonadIO m) => String -> Pass a m i o -> Pass a m i o
timed label p =
  Pass
    { runPass = \i -> do
        t0 <- liftIO getCurrentTime
        result <- runPass p i
        t1 <- liftIO getCurrentTime
        let secs = realToFrac (diffUTCTime t1 t0) :: Double
        liftIO $ writeStatusSimple (label <> ("... " <> show secs <> "s"))
        pure result
    }

pipelineWithProgress :: (MonadIO m) => ProgressRef -> Pass Metadata m [FilePath] ()
pipelineWithProgress ref =
  timedWeighted ref "Parsing" Counts.weightParsing phaseParsing
    >-> Pass{runPass = updateTotal ref}
    >-> timedWeighted ref "Preflight" Counts.weightPreflight phasePreflight
    >-> perModulePasses ref phaseTypeChecking phaseTranslation
    >-> timedWeighted ref "Kernel translate" Counts.weightKernelTranslate (mapPass passKernelTranslate)
    >-> timedWeighted ref "Kernel codegen" Counts.weightKernelCodegen passKernelCodegen
    >-> timedWeighted ref "Linking" Counts.weightLinking passLinking

timedWeighted :: (MonadIO m) => ProgressRef -> String -> Int -> Pass a m i o -> Pass a m i o
timedWeighted ref label weight p =
  Pass
    { runPass = \i -> do
        t0 <- liftIO getCurrentTime
        result <- runPass p i
        t1 <- liftIO getCurrentTime
        let secs = realToFrac (diffUTCTime t1 t0) :: Double
        liftIO $ do
          modifyIORef' ref (\(done, total) -> (done + weight, total))
          writeStatus ref (label <> ("... " <> show secs <> "s"))
        pure result
    }

perModulePasses :: (MonadIO m) => ProgressRef -> Pass Metadata m i o -> Pass Metadata m o p -> Pass Metadata m [BuildEnvelope i] [BuildEnvelope p]
perModulePasses ref p1 p2 =
  Pass
    { runPass = traverse runOne
    }
 where
  sourceStage =
    liftPass
      ( timedWeightedPerModule ref "Type checking" Counts.weightTypeChecking p1
          >-> timedWeightedPerModule ref "Translation" Counts.weightTranslation p2
      )
  cachedStage = liftPass (p1 >-> p2)
  runOne env = case env of
    BSource _ -> runPass sourceStage env
    BCached _ -> runPass cachedStage env

timedWeightedPerModule :: (MonadIO m) => ProgressRef -> String -> Int -> Pass Metadata m i o -> Pass Metadata m i o
timedWeightedPerModule ref label weight p =
  Pass
    { runPass = \i -> do
        t0 <- liftIO getCurrentTime
        result <- runPass p i
        t1 <- liftIO getCurrentTime
        let secs = realToFrac (diffUTCTime t1 t0) :: Double
        liftIO $ do
          modifyIORef' ref (\(done, total) -> (done + weight, total))
          writeStatus ref (label <> ("... " <> show secs <> "s"))
        pure result
    }

updateTotal :: (MonadIO m) => ProgressRef -> [BuildEnvelope a] -> CompilerT Metadata m [BuildEnvelope a]
updateTotal ref envelopes = do
  let numSource = length [() | BSource _ <- envelopes]
      moduleWeight = Counts.weightTypeChecking + Counts.weightTranslation
      globalWeight = Counts.weightParsing + Counts.weightPreflight + Counts.weightKernelTranslate + Counts.weightKernelCodegen + Counts.weightLinking
      total = globalWeight + numSource * moduleWeight
  -- Reset both counters: parsing is already done, so done = parsing weight.
  liftIO $ modifyIORef' ref (\(_, _) -> (Counts.weightParsing, total))
  pure envelopes

phaseMainPasses :: (MonadIO m) => Pass a m i o -> Pass a m [BuildEnvelope i] [BuildEnvelope o]
phaseMainPasses = mapPass . liftPass

compileWithCFiles :: CompilerConfig -> [FilePath] -> [FilePath] -> IO ()
compileWithCFiles config files cFiles = do
  ref <- newIORef (0, 0)
  let go = runCompilerT emptyCompilerEnvironment $ do
        setConfigC config{configCFiles = configCFiles config <> cFiles}
        runPass (pipelineWithProgress ref) files
  res <- go
  case res of
    (e, CompilerState{compilerSources}, es) -> do
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
          liftIO $ writeStatus ref ("Executable written to: " <> configExecutableName config)
          hPutStr stderr "\n"

compile :: CompilerConfig -> [FilePath] -> IO ()
compile config files = compileWithCFiles config files []

prettyRule :: InferenceRule Kind a -> Text
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
    RuleApplication _ t1 ts ->
      "Cannot apply function of type " <> prettyType (normalizeTypeIndexes t1) <> " to arguments of types " <> Text.intercalate ", " ((prettyType . normalizeTypeIndexes) <$> ts)
    RuleIfCondition _ t1 ->
      "Condition must have type `bool`, but got type " <> prettyType (normalizeTypeIndexes t1)
    RuleIfBranches _ t1 t2 ->
      "If branches must have the same type, but got " <> prettyType (normalizeTypeIndexes t1) <> " and " <> prettyType (normalizeTypeIndexes t2)
    RuleLetBindingPattern _ t1 t2 ->
      "Let binding pattern type " <> prettyType (normalizeTypeIndexes t1) <> " doesn't match expression type " <> prettyType (normalizeTypeIndexes t2)
    RuleMatchClauseGuard _ ->
      "Match clause guard must have type `bool`."
    RuleMatchClauseExpressions _ ->
      "All match clause expressions must have the same type."
    RuleListConstructor _ t1 s ->
      "List constructor type " <> prettyType (normalizeTypeIndexes t1) <> " doesn't match expected type " <> prettyType s
    RuleMatchClausePatterns _ ->
      "All match clause patterns must have the same type."
    RuleOperator _ ->
      "Operator application type error."
    RuleTopLevelFunction _ ->
      "Top-level function type error."
    RuleTopLevelConstant _ ->
      "Top-level constant type error."
    RuleAsConstraint _ ->
      "Type constraint in 'as' pattern doesn't match."
    RuleFoldType _ ->
      "Fold expression type error."
    RuleDataConstructor _ name t1 s ->
      "Data constructor `" <> name <> "` type " <> prettyType (normalizeTypeIndexes t1) <> " doesn't match expected type " <> prettyType s
    RuleSelectEquality _ t1 t2 ->
      "Record select type " <> prettyType (normalizeTypeIndexes t1) <> " doesn't match expected type " <> prettyType (normalizeTypeIndexes t2)
    RuleRecordField _ name t1 ->
      "Record field `" <> name <> "` type error: " <> prettyType (normalizeTypeIndexes t1)
    RuleRecordLacks _ name t1 ->
      "Record lacks field `" <> name <> "`: " <> prettyType (normalizeTypeIndexes t1)
    RuleTailRow _ t1 t2 ->
      "Row tail type " <> prettyType (normalizeTypeIndexes t1) <> " doesn't match " <> prettyType (normalizeTypeIndexes t2)
    RuleEntrypoint _ t1 ->
      "Entrypoint type " <> prettyType (normalizeTypeIndexes t1) <> " is invalid. Main function must return `IO<unit>`."
    RuleTraitInstance _ t1 s ->
      "Trait instance type " <> prettyType (normalizeTypeIndexes t1) <> " doesn't match expected type " <> prettyType s
    RuleAssumptionExplicit _ t1 s ->
      "Explicit type assumption " <> prettyType (normalizeTypeIndexes t1) <> " doesn't match expected type " <> prettyType s

prettyType :: (CoalPretty t) => t -> Text
prettyType p = "`" <> (renderStrict . layoutPretty defaultLayoutOptions $ prettyCoal p) <> "`"

prettyConstraintsGenError :: ConstraintsGenError a -> Text
prettyConstraintsGenError =
  \case
    EIllFormedTypeAnnotation (EAnnotationNonDistinctParameter _ name) ->
      "Type annotation is too general: error in the parameter '" <> name <> "'."
    EIllFormedTypeAnnotation (EAnnotationKindMismatch _) ->
      "Type annotation has a kind mismatch."
    EIllFormedTypeAnnotation (EAnnotationConstructor _ name) ->
      "Type constructor '" <> name <> "' is not in scope."
    EIllFormedTypeAnnotation (EAnnotationMonomorphicType _ name t) ->
      "Type parameter '" <> name <> "' resolves to concrete type " <> prettyType (normalizeTypeIndexes t) <> " instead of being polymorphic."
    EFoldPatternInRegularMatch _ ->
      "Fold patterns are not supported in regular match expression clauses. Perhaps you intended to use a 'fold'?"
    ENoDataConstructor _ name ->
      "Data constructor '" <> name <> "' not in scope."
    EDataConstructorArityMismatch _ name expected actual ->
      "Data constructor '" <> name <> "' expects " <> Text.pack (show expected) <> " arguments, but got " <> Text.pack (show actual) <> "."

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
    NoModuleMain missing ->
      "No entry point module '" <> missing <> "'"
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
    DuplicateTypeName name kind erl ->
      errorMessage ["'" <> name <> "' is already in scope as a " <> kind] env erl
    ConflictingImports name path erl ->
      errorMessage ["'" <> name <> "' conflicts with an earlier import from '" <> principalPath path <> "'"] env erl
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
    UnboundTypeVariable var typeName params erl ->
      errorMessage
        [ "Type variable '" <> var <> "' is not bound in the definition of '" <> typeName <> "'"
        , "  Declared parameters: " <> Text.intercalate ", " params
        ]
        env
        erl
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
    CallCycle cycles erl ->
      errorMessage ((\cs -> "Explicit recursion detected: " <> Text.intercalate " → " cs) <$> cycles) env erl
    MissingTraitAnnotation name traits erl ->
      errorMessage
        [ "Missing trait annotation for '" <> name <> "'"
        , "Required traits: " <> Text.intercalate ", " (prettyType <$> traits)
        , "When type parameters are explicitly annotated, all required trait constraints must be included in the annotation."
        ]
        env
        erl
    NamedFoldNotAllowed erl ->
      errorMessage ["Named fold pattern inside expression fold."] env erl

errorMessage :: [Text] -> Environment Text -> ErrorLocation Metadata -> Text
errorMessage msgs env (ErrorLocation path loc) =
  maybe
    -- If source not found, return a basic error message
    (Text.unlines $ ("Internal compiler error: source file not found for module '" <> path <> "'") : msgs)
    (prettyErrorMessage (("\n• " <>) <$> msgs) loc)
    (Environment.lookup path env)
