{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler (pipeline, compile, prettyError) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Config (CompilerConfig)
import Coal.Compiler.Environment
import Coal.Compiler.Error (errorLocation)
import Coal.Compiler.Pass (Pass (..), (>->))
import Coal.Compiler.Pass.LoweringPhase (loweringPhase)
import Coal.Compiler.Pass.ParsingPhase (parsingPhase)
import Coal.Compiler.Pass.PreflightPhase (preflightPhase)
import Coal.Compiler.Pass.TranslationPhase (translationPhase)
import Coal.Compiler.Pass.TypePhase (typePhase)
import Coal.Compiler.Stack
import Coal.Compiler.TypeInference.Errors (prettyErrorMessage)
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Megaparsec (errorBundlePretty)

pipeline :: (MonadIO m) => Pass Metadata m [FilePath] ()
pipeline =
  parsingPhase
    >-> preflightPhase
    >-> typePhase
    >-> translationPhase
    >-> loweringPhase

compile :: CompilerConfig -> [FilePath] -> IO ()
compile config files = do
  (e, CompilerState{..}, es) <- runCompilerT emptyCompilerEnvironment $ do
    setConfigC config
    runPass pipeline files
  forM_ es $
    \err -> do
      case errorLocation err of
        Just (ErrorLocation name _) ->
          Text.putStrLn ("In module '" <> name <> "':\n")
        Nothing ->
          pure ()
      Text.putStrLn (prettyError compilerVerbatimSource err)
  case e of
    Left e1 ->
      print e1
    Right{} -> do
      pure ()

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
      -- TODO
      errorMessage ["Type error: " <> Text.pack (show rule)] env erl
    NameNotInScope name erl ->
      errorMessage ["Name not in scope: '" <> name <> "'"] env erl
    ConstraintsError e erl ->
      errorMessage ["TODO: " <> Text.pack (show e)] env erl
    NonExhaustivePatterns erl ->
      errorMessage ["Non-exhaustive patterns"] env erl
    FoldPatternInRegularMatch erl ->
      errorMessage ["Fold pattern cannot appear in regular match expression"] env erl
    FoldPatternOutsideConstructor erl ->
      errorMessage ["Fold pattern cannot appear outside constructor"] env erl
    Shadowing name erl ->
      errorMessage ["Name shadowing: '" <> name <> "'"] env erl
    MissingInstance trait erl ->
      -- TODO
      errorMessage ["Missing trait instance: '" <> Text.pack (show trait) <> "'"] env erl
    NameAlreadyDefined name erl ->
      errorMessage ["Name already defined: '" <> name <> "'"] env erl
    ConflictingParameter name erl ->
      errorMessage ["Conflicting parameter name: '" <> name <> "'"] env erl
    NameNotInModule name module_ erl ->
      errorMessage ["The module " <> module_ <> " does not export '" <> name <> "'"] env erl

errorMessage :: [Text] -> Environment Text -> ErrorLocation Metadata -> Text
errorMessage msg env (ErrorLocation path loc) =
  case Environment.lookup path env of
    Just src ->
      prettyErrorMessage msg src loc
    _ ->
      error "Implementation error"
