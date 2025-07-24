{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler where

import Control.Monad ((>=>))
import Control.Monad.Reader (Reader, asks, runReader)
import Control.Monad.State (get, gets, runState)
import Data.Data (Data)
import Lang.Utils (Name, forM)
import Noll.Compiler.Transform.Dictionaries
import Noll.Compiler.Lowpass.TranslateModule (translateModule)
import Noll.Compiler.NormalizeObjects (NormalizeObjectsTransformContext (..))
import Noll.Compiler.PatternMatching
import Noll.Compiler.PatternMatching.Rule (MatchMonad (..), runMatchMonad)
import Noll.Compiler.Stack
import Noll.Compiler.Transform.Fold
import Noll.Compiler.Transform.Nats
import Noll.Compiler.Transform.Pattern.AsDesugar
import Noll.Compiler.Transform.Pattern.Desugar
import Noll.Compiler.Transform.Pattern.OrExpansion
import Noll.Compiler.Transform.Type.AliasExpansion
import Noll.Compiler.Transform.Unfold
import Noll.Compiler.TypeInference
import Noll.Language
import Noll.Module (Module (..))
import Noll.Module.Constant
import Noll.Module.Definition
import Noll.TypeSystem.Substitution (normalizeTypeIndexes)

import qualified Lang.Lowpass.Language as Lowpass
import qualified Noll.Compiler.Lowpass.Environment as Lowpass

withSupplyC :: (Monad m) => (Int -> (c, Int)) -> CompilerT a m c
withSupplyC f = do
  n <- gets compiler2Supply
  let (r, n') = f n
  insertSupplyC n'
  pure r

aliasExpansionTrans :: (Monad m) => (c -> Reader AliasEnvironment c) -> c -> CompilerT a m c
aliasExpansionTrans f e = asks (runReader (f e) . compiler2AliasEnv)

expandAliasesC :: (Monad m, Data a) => Module a Kind () -> CompilerT a m (Module a Kind ())
expandAliasesC = aliasExpansionTrans expandAliases

foldExpansionTrans :: (Monad m) => (c -> FoldExpansion c) -> c -> CompilerT a m c
foldExpansionTrans f e = withSupplyC (\n -> runFoldExpansion "fold" n (f e))

compileUnfoldsC :: (Monad m, Data a, Monoid a) => Module a Kind () -> CompilerT a m (Module a Kind ())
compileUnfoldsC = foldExpansionTrans compileFolds

unfoldExpansionTrans :: (Monad m) => (c -> UnfoldExpansion c) -> c -> CompilerT a m c
unfoldExpansionTrans f e = withSupplyC (\n -> runUnfoldExpansion "unfold" n (f e))

compileFoldsC :: (Monad m, Data a, Monoid a) => Module a Kind () -> CompilerT a m (Module a Kind ())
compileFoldsC = unfoldExpansionTrans compileUnfolds

indexedC :: (Monad m, Traversable t) => t e -> CompilerT a m (t IndexedType)
indexedC t = withSupplyC (runState (indexed t))

runTypeInferenceC :: (Monad m, Data a, Eq a, Show a) => Module a Kind () -> CompilerT a m (Module a Kind IndexedType)
runTypeInferenceC m = do
  defs <- traverse indexedC ds
  (tdefs, _) <- typeDefinitionsC defs
  pure (Module p ns (normalizeTypeIndexes tdefs))
 where
  Module p ns ds = m

normalizeObjectC :: (Monad m, NormalizeObjectsTransformContext c) => c -> CompilerT a m c
normalizeObjectC = pure . normalizeObject

denormalizeObjectC :: (Monad m, NormalizeObjectsTransformContext c) => c -> CompilerT a m c
denormalizeObjectC = pure . denormalizeObject

patternDesugarTrans :: (Monad m) => (c -> PatternDesugar s TypeIndex Kind c) -> c -> CompilerT a m c
patternDesugarTrans f e = withSupplyC (\n -> runPatternDesugar "v" n (f e))

desugarPatternsC :: (Monad m, Sugared s TypeIndex Kind c) => c -> CompilerT a m c
desugarPatternsC = patternDesugarTrans desugarPatterns

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
  env1 <- gets compiler2NameStore
  env2 <- asks compiler2InstanceEnvironment
  withSupplyC (\n -> runDictionaryStack (DictionaryEnvironment env1 env2) n (f e))

placeholderInsertionC :: (Monad m, Monoid a, Data a, Show a) => Module a Kind IndexedType -> CompilerT a m (Module a Kind IndexedType)
placeholderInsertionC (Module p ns ds) = do
  es <- forM ds $
    \case
      d@(DConstant name c) -> do
        d1 <- placeholderTrans expandTraits d
        case d1 of
          DConstant _ (Constant _ (With ts t) e) -> do
            insertNameC name (Forall (typeIndexesIn t) ts t)
          _ ->
            error "Implementation error"
        pure d1
      d ->
        placeholderTrans expandTraits d
  pure (Module p ns es)

lowpassMonadTrans :: (Monad m) => (c -> Reader Lowpass.TranslateEnvironment d) -> c -> CompilerT a m d
lowpassMonadTrans f e = pure (runReader (f e) (Lowpass.initialTranslateEnvironment mempty))

lowpassTranslationC :: (Show a, Monad m, Data a) => Module a Kind IndexedType -> CompilerT a m (Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type))
lowpassTranslationC = lowpassMonadTrans translateModule

--

typePass :: (Monad m, Monoid a, Data a, Eq a, Show a) => Module a Kind () -> CompilerT a m (Module a Kind IndexedType)
typePass =
  -- Expand type aliases
  expandAliasesC
    -- Expand unfolds (codata)
    >=> compileUnfoldsC
    -- Expand folds
    >=> compileFoldsC
    -- Type inference
    >=> runTypeInferenceC

mainPass :: (Monad m, Monoid a, Data a, Show a) => Module a Kind IndexedType -> CompilerT a m (Module a Kind IndexedType)
mainPass =
  -- Normalize top-level expressions
  normalizeObjectC
    -- Translate patterns in expression bindings to match expressions
    >=> desugarPatternsC
    -- Compile or-patterns
    >=> compileOrPatterns
    --    -- Translate record patterns to select operators
    --    >=> TODO
    -- Compile as-patterns
    >=> pure . desugarAsPatterns
    -- Compile match statements
    >=> compileMatchExprsC
    -- Placeholder insertion
    >=> placeholderInsertionC
    -- Denormalize top-level expressions
    >=> denormalizeObjectC
    -- Expand nats
    >=> compileNatsC

compileModule :: (Monad m, Monoid a, Data a, Eq a, Show a) => Module a Kind () -> CompilerT a m (Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type))
compileModule =
  typePass
    >=> mainPass
    -- Final lowering
    >=> lowpassTranslationC

compileModule_ :: (Monad m, Monoid a, Data a, Eq a, Show a) => Module a Kind () -> CompilerT a m (Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type))
compileModule_ m = do
  r <- compileModule m
  s <- get
  pure r

--  traceShow s $
--    pure r
