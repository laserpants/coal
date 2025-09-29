{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Environment (overCompilerDictionaryNameEnvironment)
import Coal.Compiler.Kernel.TranslateModule (translateModule)
import Coal.Compiler.PatternMatching
import Coal.Compiler.Stack
import Coal.Compiler.Transform.Definition.Fold
import Coal.Compiler.Transform.Definition.Unfold
import Coal.Compiler.Transform.Dictionaries
import Coal.Compiler.Transform.Fold
import Coal.Compiler.Transform.LambdaMatch
import Coal.Compiler.Transform.Nats
import Coal.Compiler.Transform.NormalizeObjects (NormalizeObjectsTransformContext (..))
import Coal.Compiler.Transform.Pattern.AsDesugar
import Coal.Compiler.Transform.Pattern.Desugar
import Coal.Compiler.Transform.Pattern.OrExpansion
import Coal.Compiler.Transform.Pattern.RecordDesugar
import Coal.Compiler.Transform.PatternExhaustiveCheck
import Coal.Compiler.Transform.Type.AliasExpansion (AliasContext (..))
import Coal.Compiler.Transform.Unfold
import Coal.Compiler.TypeInference
import Coal.Graphviz.Dot (writeDotFile)
import qualified Coal.Kernel.Language as Kernel
import Coal.Language
import Coal.Language.Module (Module (..), overModuleDefinitionsM)
import Coal.Language.Module.Definition
import Coal.Language.Module.Definition.Constant
import Coal.Language.Module.Definition.Instance
import Coal.TypeSystem.Substitution (normalizeTypeIndexes)
import Control.Monad.Except
import Control.Monad.Reader (local)
import Control.Monad.State (gets, runState)
import Data.Data (Data)
import Data.Text (Text)
import qualified Data.Text as Text
import Extra (Name)
import Prettyprinter (Pretty (..))

withSupplyC :: (Monad m) => (Int -> (c, Int)) -> CompilerT a m c
withSupplyC = withSupplyMC . (pure .)

withSupplyMC :: (Monad m) => (Int -> CompilerT a m (c, Int)) -> CompilerT a m c
withSupplyMC f = do
  n <- gets compilerSupply
  (r, n') <- f n
  insertSupplyC n'
  pure r

compileTopLevelFoldsC :: (Monad m, Data a, Monoid a) => Module a Kind () -> CompilerT a m (Module a Kind ())
compileTopLevelFoldsC = overModuleDefinitionsM (traverse compileTopLevelFolds)

compileTopLevelUnfoldsC :: (Monad m, Monoid a, Data a) => Module a Kind () -> CompilerT a m (Module a Kind ())
compileTopLevelUnfoldsC = overModuleDefinitionsM (traverse compileTopLevelUnfolds)

indexedC :: (Monad m, Traversable t) => t e -> CompilerT a m (t IndexedType)
indexedC = withSupplyC . runState . indexed

runTypeInferenceC :: (MonadIO m, Data a, Eq a, Show a) => Module a Kind () -> CompilerT a m (Module a Kind IndexedType)
runTypeInferenceC m = do
  defs <- traverse indexedC ds
  void $ writeDotFilesC "indexed" (Module p ns defs)
  (tdefs, _) <- typeDefinitionsC defs
  pure (Module p ns (normalizeTypeIndexes tdefs))
 where
  Module p ns ds = m

insertPlaceholders :: (Monad m, Monoid a, Data a) => Definition a Kind IndexedType -> CompilerT a m (Definition a Kind IndexedType)
insertPlaceholders =
  \case
    d@(DConstant _ name _ _) ->
      insertTypeInfo name =<< expandInLocalEnv d
    DInstance loc name (InstanceDef ts t ds) -> do
      es <- forM ds (insertPlaceholdersInDef (Trait name t))
      pure (DInstance loc name (InstanceDef ts t es))
    d@DFold{} ->
      expandInLocalEnv d
    d@DUnfold{} ->
      expandInLocalEnv d
    d ->
      pure d

insertPlaceholdersInDef :: (Monad m, Monoid a, Data a) => Trait ParameterizedType -> Definition a Kind IndexedType -> CompilerT a m (Definition a Kind IndexedType)
insertPlaceholdersInDef trait =
  \case
    c@DConstant{} ->
      insertTypeInfo (instanceLabel trait (definitionName c)) =<< expandInLocalEnv c
    _ ->
      error "TODO"

expandInLocalEnv :: (Monad m, TraitContext a b) => b -> CompilerT a m b
expandInLocalEnv d = do
  env1 <- gets compilerNameStore
  local (overCompilerDictionaryNameEnvironment (const env1)) (expandTraits d)

insertTypeInfo :: (Monad m) => Name -> Definition a k IndexedType -> CompilerT a m (Definition a k IndexedType)
insertTypeInfo name d = do
  insertName d name
  pure d

insertName :: (Monad m) => Definition a k IndexedType -> Name -> CompilerT a m ()
insertName (DConstant _ _ (ConstantDef _ _ (With ts t) _) _) name = insertNameC name (Forall (typeIndexesIn t) ts t)
insertName _ _ = error "Implementation error"

typeCheckingPass :: (MonadIO m, Monoid a, Data a, Eq a, Show a) => Module a Kind () -> CompilerT a m (Module a Kind IndexedType)
typeCheckingPass =
  -- Expand type aliases
  expandAliases
    >=> compileTopLevelUnfoldsC
    >=> compileTopLevelFoldsC
    -- Expand unfolds (codata)
    >=> compileUnfolds
    -- Expand folds
    >=> compileFolds
    >=> writeDotFilesC "expand_folds"
    -- Lambda match expressions
    >=> compileLambdaMatch
    >=> writeDotFilesC "lambda_match"
    -- Type inference
    >=> runTypeInferenceC

mainPass :: (MonadIO m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
mainPass =
  -- Normalize top-level functions and constants
  pure . normalizeObject
    -- Translate patterns in expression bindings to match expressions
    >=> desugarPatterns
    -- Compile or-patterns
    >=> compileOrPatterns
    >=> writeDotFilesC "patterns"
    -- Translate record patterns to select operators
    >=> compileRecordPatterns
    >=> writeDotFilesC "record_patterns"
    >=> patternExhaustiveCheckM
    -- Compile as-patterns
    >=> pure . desugarAsPatterns
    >=> writeDotFilesC "as_patterns"
    -- Compile match statements
    >=> compileMatchExprs
    >=> writeDotFilesC "match_exprs"
    -- Placeholder insertion
    >=> overModuleDefinitionsM (traverse insertPlaceholders)
    -- Denormalize top-level functions and constants
    >=> pure . denormalizeObject
    -- Expand nats
    >=> compileNats

compileModule :: (MonadIO m) => Module Metadata Kind () -> CompilerT Metadata m (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))
compileModule =
  typeCheckingPass
    >=> mainPass
    -- Final lowering
    >=> translateModule

writeDotFilesC :: (MonadIO m, Pretty t, Show t) => Text -> Module a k t -> m (Module a k t)
writeDotFilesC ns m = do
  liftIO $ writeDotFiles ns m
  pure m

writeDotFiles :: (Pretty t, Show t) => Text -> Module a k t -> IO ()
writeDotFiles ns m@(Module (Path path) _ defs) = do
  writeDotFile prefix m
  forM_ defs $
    \case
      def@DFunction{} ->
        writeDotFile (prefixed $ definitionName def) def
      def@DConstant{} ->
        writeDotFile (prefixed $ definitionName def) def
      _ ->
        pure ()
 where
  prefix = ns <> "__" <> Text.intercalate "_" path
  prefixed n = prefix <> "_" <> n
