{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler where

import Coal.Compiler.Kernel.TranslateModule (translateModule)
import Coal.Compiler.PatternMatching
import Coal.Compiler.PatternMatching.Rule (MatchMonad (..), runMatchMonad)
import Coal.Compiler.Stack
import Coal.Compiler.Transform.Definition.Fold
import Coal.Compiler.Transform.Definition.Unfold
import Coal.Compiler.Transform.Dictionaries
import Coal.Compiler.Transform.Fold
import Coal.Compiler.Transform.Nats
import Coal.Compiler.Transform.NormalizeObjects (NormalizeObjectsTransformContext (..))
import Coal.Compiler.Transform.Pattern.AsDesugar
import Coal.Compiler.Transform.Pattern.Desugar
import Coal.Compiler.Transform.Pattern.OrExpansion
import Coal.Compiler.Transform.Pattern.RecordDesugar
import Coal.Compiler.Transform.Type.AliasExpansion
import Coal.Compiler.Transform.Unfold
import Coal.Compiler.Transform.WhereClauses
import Coal.Compiler.TypeInference
import Coal.Graphviz.Dot (writeDotFile)
import Coal.Language
import Coal.Language.Module (Module (..), overModuleDefinitionsM)
import Coal.Language.Module.Constant
import Coal.Language.Module.Definition
import Coal.TypeSystem.Substitution (normalizeTypeIndexes)
import Control.Monad ((>=>))
import Control.Monad.Reader (MonadIO, Reader, asks, liftIO, runReader)
import Control.Monad.State (gets, runState)
import Control.Monad.Writer (Writer, runWriter)
import Data.Data (Data)
import Data.Text (Text)
import Extra (Name, forM, forM_)
import Prettyprinter (Pretty (..))

import qualified Coal.Compiler.Kernel.Environment as Kernel
import qualified Coal.Kernel.Language as Kernel
import qualified Data.Text as Text

withSupplyC :: (Monad m) => (Int -> (c, Int)) -> CompilerT a m c
withSupplyC f = do
  n <- gets compilerSupply
  let (r, n') = f n
  insertSupplyC n'
  pure r

whereClausesExpansionTrans :: (Monad m) => (c -> Writer [(Name, Name)] c) -> c -> CompilerT a m c
whereClausesExpansionTrans f e = pure (fst $ runWriter (f e))

expandWhereClausesC :: (Monad m, Data a) => Module a Kind () -> CompilerT a m (Module a Kind ())
expandWhereClausesC = whereClausesExpansionTrans expandWhereClausesModule

aliasExpansionTrans :: (Monad m) => (c -> Reader AliasEnvironment c) -> c -> CompilerT a m c
aliasExpansionTrans f e = asks (runReader (f e) . compilerAliasEnvironment)

expandAliasesC :: (Monad m, Data a) => Module a Kind () -> CompilerT a m (Module a Kind ())
expandAliasesC = aliasExpansionTrans expandAliases

foldExpansionTrans :: (Monad m) => (c -> FoldExpansion c) -> c -> CompilerT a m c
foldExpansionTrans f e = withSupplyC (\n -> runFoldExpansion "fold" n (f e))

compileUnfoldsC :: (Monad m, Data a, Monoid a) => Module a Kind () -> CompilerT a m (Module a Kind ())
compileUnfoldsC = unfoldExpansionTrans compileUnfolds

unfoldExpansionTrans :: (Monad m) => (c -> UnfoldExpansion c) -> c -> CompilerT a m c
unfoldExpansionTrans f e = withSupplyC (\n -> runUnfoldExpansion "unfold" n (f e))

compileFoldsC :: (Monad m, Data a, Monoid a) => Module a Kind () -> CompilerT a m (Module a Kind ())
compileFoldsC = foldExpansionTrans compileFolds

compileTopLevelFoldsC :: (Monad m, Data a, Monoid a) => Module a Kind () -> CompilerT a m (Module a Kind ())
compileTopLevelFoldsC = foldExpansionTrans (overModuleDefinitionsM (traverse compileTopLevelFolds))

compileTopLevelUnfoldsC :: (Monad m) => Module a Kind () -> CompilerT a m (Module a Kind ())
compileTopLevelUnfoldsC = unfoldExpansionTrans (overModuleDefinitionsM (traverse compileTopLevelUnfolds))

indexedC :: (Monad m, Traversable t) => t e -> CompilerT a m (t IndexedType)
indexedC t = withSupplyC (runState (indexed t))

runTypeInferenceC :: (MonadIO m, Data a, Eq a, Show a) => Module a Kind () -> CompilerT a m (Module a Kind IndexedType)
runTypeInferenceC m = do
  defs <- traverse indexedC ds

  liftIO $ writeDotFiles "indexed" (Module p ns defs)

  (tdefs, _) <- typeDefinitionsC defs
  pure (Module p ns (normalizeTypeIndexes tdefs))
 where
  Module p ns ds = m

normalizeObjectsC :: (Monad m, NormalizeObjectsTransformContext c) => c -> CompilerT a m c
normalizeObjectsC = pure . normalizeObject

denormalizeObjectsC :: (Monad m, NormalizeObjectsTransformContext c) => c -> CompilerT a m c
denormalizeObjectsC = pure . denormalizeObject

patternDesugarTrans :: (Monad m) => (c -> PatternDesugar s TypeIndex Kind c) -> c -> CompilerT a m c
patternDesugarTrans f e = withSupplyC (\n -> runPatternDesugar "v" n (f e))

desugarPatternsC :: (Monad m, Sugared s TypeIndex Kind c) => c -> CompilerT a m c
desugarPatternsC = patternDesugarTrans desugarPatterns

recordPatternDesugarTrans :: (Monad m) => (c -> RecordDesugarStack a c) -> c -> CompilerT a m c
recordPatternDesugarTrans f e = withSupplyC (evalRecordDesugarStack (f e) "row")

recordPatternDesugarC :: (Monad m, Monoid a, Data a) => Module a Kind IndexedType -> CompilerT a m (Module a Kind IndexedType)
recordPatternDesugarC = recordPatternDesugarTrans compileRecordPatterns

matchMonadTrans :: (Monad m) => (c -> MatchMonad c) -> c -> CompilerT a m c
matchMonadTrans f e = withSupplyC (\n -> runMatchMonad "match" n (f e))

compileMatchExprsC :: (Monad m, MatchExpressionContext c) => c -> CompilerT a m c
compileMatchExprsC = matchMonadTrans compileMatchExprs

natExpansionTrans :: (Monad m) => (c -> NatExpansion c) -> c -> CompilerT a m c
natExpansionTrans f e = withSupplyC (\n -> runNatExpansion "succ" n (f e))

compileNatsC :: (Monad m, Monoid a, Data a) => Module a Kind IndexedType -> CompilerT a m (Module a Kind IndexedType)
compileNatsC = natExpansionTrans compileNats

placeholderTrans :: (Monad m) => (c -> DictionaryStack c) -> c -> CompilerT a m c
placeholderTrans f e = do
  env1 <- gets compilerNameStore
  env2 <- asks compilerInstanceEnvironment
  withSupplyC (\n -> runDictionaryStack (DictionaryEnvironment env1 env2) n (f e))

placeholderInsertionC :: (Monad m, Monoid a, Data a) => Module a Kind IndexedType -> CompilerT a m (Module a Kind IndexedType)
placeholderInsertionC = overModuleDefinitionsM (traverse go)
 where
  go =
    \case
      d@(DConstant _ name _ _) -> do
        d1 <- placeholderTrans expandTraits d
        case d1 of
          DConstant _ _ (Constant _ (With ts t) _) _ ->
            insertNameC name (Forall (typeIndexesIn t) ts t)
          _ ->
            error "Implementation error"
        pure d1
      --      DFold{} ->
      --        TODO
      --      DUnfold{} ->
      --        TODO
      DAnnotation t d ->
        DAnnotation t <$> go d
      DInstance name ts1 t1 ds -> do
        es <- forM ds $
          \case
            c@(DConstant _ dname _ _) -> do
              c1 <- placeholderTrans expandTraits c
              case c1 of
                DConstant _ _ (Constant _ (With ts t) _) _ -> do
                  let trait = Trait name t1
                      name1 = dname <> "__$instance_" <> serialize trait
                  insertNameC name1 (Forall (typeIndexesIn t) ts t)
                _ ->
                  error "Implementation error"
              pure c1
            _ ->
              error "TODO"
        pure (DInstance name ts1 t1 es)
      d ->
        pure d

kernelMonadTrans :: (Monad m) => (c -> Reader Kernel.KernelEnvironment d) -> c -> CompilerT a m d
kernelMonadTrans f e = pure (runReader (f e) (Kernel.initialKernelEnvironment mempty))

kernelTranslationC :: (Show a, Monad m, Data a) => Module a Kind IndexedType -> CompilerT a m (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))
kernelTranslationC = kernelMonadTrans translateModule

typeCheckingPass :: (MonadIO m, Monoid a, Data a, Eq a, Show a) => Module a Kind () -> CompilerT a m (Module a Kind IndexedType)
typeCheckingPass =
  -- Expand type aliases
  expandAliasesC
    --    >=> compileTopLevelUnfoldsC
    >=> compileTopLevelFoldsC
    -- Expand unfolds (codata)
    >=> compileUnfoldsC
    -- Expand folds
    >=> compileFoldsC
    >=> writeDotFilesC "expand_folds"
    -- Type inference
    >=> runTypeInferenceC

mainPass :: (Eq a, MonadIO m, Monoid a, Data a, Show a) => Module a Kind IndexedType -> CompilerT a m (Module a Kind IndexedType)
mainPass =
  -- Normalize top-level functions and constants
  normalizeObjectsC
    -- Translate patterns in expression bindings to match expressions
    >=> desugarPatternsC
    -- Compile or-patterns
    >=> compileOrPatterns
    >=> writeDotFilesC "patterns"
    -- Translate record patterns to select operators
    >=> recordPatternDesugarC
    >=> writeDotFilesC "record_patterns"
    -- Compile as-patterns
    >=> pure . desugarAsPatterns
    >=> writeDotFilesC "as_patterns"
    -- Compile match statements
    >=> compileMatchExprsC
    >=> writeDotFilesC "match_exprs"
    -- Placeholder insertion
    >=> placeholderInsertionC
    -- Denormalize top-level functions and constants
    >=> denormalizeObjectsC
    -- Expand nats
    >=> compileNatsC

compileModule :: (MonadIO m, Monoid a, Data a, Eq a, Show a) => Module a Kind () -> CompilerT a m (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))
compileModule =
  typeCheckingPass
    >=> mainPass
    -- Final lowering
    >=> kernelTranslationC

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
      def@DAnnotation{} ->
        writeDotFile (prefixed $ definitionName def) def
      _ ->
        pure ()
 where
  prefix = ns <> "__" <> Text.intercalate "_" path
  prefixed n = prefix <> "_" <> n
