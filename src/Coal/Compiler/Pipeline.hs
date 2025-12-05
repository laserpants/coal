{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pipeline (pipeline, compile, compileWithCFiles, prettyError) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Config (CompilerConfig (..))
import Coal.Compiler.Environment (emptyCompilerEnvironment)
import Coal.Compiler.Error (errorLocation)
import Coal.Compiler.Pass (Pass (..), (>->))
import Coal.Compiler.Pass.LoweringPhase (loweringPhase)
import Coal.Compiler.Pass.MainPhase (mainPhase)
import Coal.Compiler.Pass.ParsingPhase (parsingPhase)
import Coal.Compiler.Pass.ParsingPhase.Parsing (embedded)
import Coal.Compiler.Pass.PreflightPhase (preflightPhase)
import Coal.Compiler.Stack
import Coal.Compiler.TypeInference.Errors (prettyErrorMessage)
import Coal.Language (Kind)
import Coal.Language.Module.Path (principalPath)
import Coal.TypeSystem.Constraint.Generation
import Coal.TypeSystem.Constraint.Generation.Internal
import Coal.TypeSystem.Substitution (normalizeTypeIndexes)
import Control.Monad.Except (MonadIO, forM_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import System.Console.AsciiProgress
import Text.Megaparsec (errorBundlePretty)

pipeline :: (MonadIO m) => Pass Metadata m [FilePath] ()
pipeline =
  parsingPhase
    >-> preflightPhase
    >-> mainPhase
    >-> loweringPhase

compileWithCFiles :: CompilerConfig -> [FilePath] -> [FilePath] -> IO ()
compileWithCFiles config files cFiles = do
  if configSilent config
    then go Nothing
    else do
      displayConsoleRegions $ do
        pb <-
          newProgressBar
            def
              { pgTotal = (fromIntegral (length embedded + length files) * 73) + 28
              , pgWidth = 100
              , pgFormat = "Compiling [:bar] :percent"
              }
        go (Just pb)
 where
  go progressBar = do
    (e, CompilerState{..}, es) <- runCompilerT (emptyCompilerEnvironment progressBar) $ do
      setConfigC config{configCFiles = configCFiles config <> cFiles}
      runPass pipeline files
    forM_ es $
      \err -> do
        case errorLocation err of
          Just (ErrorLocation name _) ->
            putStrLn ("In module '" <> Text.unpack name <> "':\n")
          Nothing ->
            pure ()
        Text.putStrLn (prettyError compilerVerbatimSource err)
    case e of
      Left e1 ->
        print e1
      Right{} -> do
        pure ()

compile :: CompilerConfig -> [FilePath] -> IO ()
compile config files = compileWithCFiles config files []

prettyRule :: (Show a) => InferenceRule Kind a -> Text
prettyRule =
  \case
    RuleAnnotation _ t1 _ ->
      "Type annotation doesn't match inferred type, namely " <> prettyType u1
     where
      u1 = normalizeTypeIndexes t1
    RuleLetImplicit _ _ t1 t2 ->
      "Cannot unify " <> prettyType u1 <> " with " <> prettyType u2
     where
      u1 = normalizeTypeIndexes t1
      u2 = normalizeTypeIndexes t2
    RuleTypeConstraint _ name t1 s ->
      "Cannot unify " <> "'" <> name <> "' : " <> prettyType s <> " with expected type " <> prettyType u1
     where
      u1 = normalizeTypeIndexes t1
    RuleUnfoldExplicit _ t1 s ->
      "Cannot unify " <> prettyType s <> " with expected type " <> prettyType u1
     where
      u1 = normalizeTypeIndexes t1
    RuleCodataRecord _ t1 s ->
      "Cannot unify " <> prettyType s <> " with expected type " <> prettyType u1
     where
      u1 = normalizeTypeIndexes t1
    e ->
      Text.pack ("TODO: " <> show e)

-- TODO: rename
prettyType :: (Pretty t) => t -> Text
prettyType p = "{" <> prettyType_ p <> "}"

-- TODO: rename
prettyType_ :: (Pretty t) => t -> Text
prettyType_ p = renderStrict . layoutPretty defaultLayoutOptions $ pretty p

prettyConstraintsGenError :: (Show a) => ConstraintsGenError a -> Text
prettyConstraintsGenError =
  \case
    EIllFormedTypeAnnotation (EAnnotationNonDistinctParameter _ name) ->
      "Type annotation is too general: error in the parameter '" <> name <> "'"
    ECodataFieldMismatch _ ->
      "Codata type field mismatch"
    EFoldPatternInRegularMatch _ ->
      "Fold patterns are not allowed in regular match clauses"
    ENoDataConstructor _ name ->
      "Data constructor '" <> name <> "' not in scope"
    e ->
      Text.pack ("TODO:" <> show e)

prettyError :: Environment Text -> CompilerError Metadata -> Text
prettyError env =
  \case
    ParserError file err ->
      "In file \"" <> Text.pack file <> "\":\n\n" <> Text.pack (errorBundlePretty err)
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
    NameNotInModule name path erl ->
      errorMessage ["The module " <> principalPath path <> " does not export '" <> name <> "'"] env erl
    MissingType name path erl ->
      errorMessage ["The module " <> principalPath path <> " does not export a type '" <> name <> "'"] env erl
    MissingCotype name path erl ->
      errorMessage ["The module " <> principalPath path <> " does not export a coata type '" <> name <> "'"] env erl
    NoDataConstructorForType ctor name _ erl ->
      errorMessage ["No constructor '" <> ctor <> "' for type '" <> name <> "' in scope"] env erl
    NoCodataAccessorForCotype xsor name _ erl ->
      errorMessage ["No field '" <> xsor <> "' for codata type '" <> name <> "' in scope"] env erl
    TraitNotInScope trait erl ->
      errorMessage ["No trait '" <> trait <> "' in scope"] env erl
    MissingTraitDefinition name trait erl ->
      errorMessage ["A defintion for '" <> name <> "' is missing from the instance for trait '" <> trait <> "'"] env erl
    UnexpectedTraitDefinition name trait erl ->
      errorMessage ["The trait '" <> trait <> "' does not have an entry '" <> name <> "'"] env erl
    MissingRequiredInstance name t erl ->
      errorMessage ["Missing required instance for trait '" <> name <> "<" <> prettyType_ t <> ">'"] env erl

errorMessage :: [Text] -> Environment Text -> ErrorLocation Metadata -> Text
errorMessage msg env (ErrorLocation path loc) =
  case Environment.lookup path env of
    Just src ->
      prettyErrorMessage msg src loc
    _ ->
      error "Implementation error"
