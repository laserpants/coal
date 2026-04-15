{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TranslationPhase.ExpandOrPatterns (
  passExpandOrPatterns,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.FreeVars (boundIn)
import Coal.Common.Label (Label (..))
import Coal.Compiler.Journal
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language
import Coal.Language.Module
import Coal.Language.Module.Path
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.State (gets)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformBiM)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup (sconcat)
import Extras (Map, traverseM)

passExpandOrPatterns :: (Monad m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passExpandOrPatterns = Pass{runPass = passImpl}

passImpl :: (Monad m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
passImpl m = do
  setCurrentModuleC m
  transformBiM expandExpression m

expandExpression :: (Monad m) => Expression Metadata Kind IndexedType -> CompilerT Metadata m (Expression Metadata Kind IndexedType)
expandExpression =
  \case
    EMatch a t e cs ->
      EMatch a t e . sconcat <$> traverse expandOrPatterns cs
    EFold a t es cs ->
      EFold a t es . sconcat <$> traverse expandOrPatterns cs
    e ->
      pure e

class PatternContext a where
  expandOrPatterns :: (Monad m) => a -> CompilerT Metadata m (NonEmpty a)

instance (PatternContext a) => PatternContext [a] where
  expandOrPatterns = traverseM expandOrPatterns

instance (PatternContext a) => PatternContext (NonEmpty a) where
  expandOrPatterns = traverseM expandOrPatterns

instance (PatternContext a) => PatternContext (Map k a) where
  expandOrPatterns = traverseM expandOrPatterns

instance (PatternContext a) => PatternContext (Maybe a) where
  expandOrPatterns = traverseM expandOrPatterns

instance (Data t) => PatternContext (Clause Metadata Kind t) where
  expandOrPatterns =
    \case
      EClause a p cs -> do
        q1 :| qs <- expandOrPatterns p
        pure (EClause a q1 cs :| [EClause a q cs | q <- qs])

instance (Data t) => PatternContext (Pattern Metadata Kind t) where
  expandOrPatterns =
    \case
      POr loc _ p1 p2 -> do
        this <- gets (principalPath . compilerCurrentPath)
        let vars1 = boundIn p1
            vars2 = boundIn p2
        when (vars1 /= vars2) $ do
          tellErrors [OrPatternVariableMismatch vars1 vars2 (ErrorLocation this loc)]
          -- TODO: ??
          throwError PreflightFailure
        qs1 <- expandOrPatterns p1
        qs2 <- expandOrPatterns p2
        pure (qs1 <> qs2)
      PAnnotation a t p -> do
        q1 :| qs <- expandOrPatterns p
        pure (PAnnotation a t q1 :| [PAnnotation a t q | q <- qs])
      PTuple a t ps -> do
        qs1 :| qss <- expandOrPatterns ps
        pure (PTuple a t qs1 :| [PTuple a t qs | qs <- qss])
      PConstructor a (Label t name) ps -> do
        qs1 :| qss <- expandOrPatterns ps
        pure (PConstructor a (Label t name) qs1 :| [PConstructor a (Label t name) qs | qs <- qss])
      PListLiteral a t ps -> do
        qs1 :| qss <- expandOrPatterns ps
        pure (PListLiteral a t qs1 :| [PListLiteral a t qs | qs <- qss])
      PListCons a t p1 p2 -> do
        qs1 <- expandOrPatterns p1
        qs2 <- expandOrPatterns p2
        pure (PListCons a t <$> qs1 <*> qs2)
      PRecord a t d mp -> do
        dicts <- NonEmpty.toList . sequence <$> traverse expandOrPatterns d
        pure (NonEmpty.fromList [PRecord a t d1 mp | d1 <- dicts])
      PAs a ll p -> do
        q1 :| qs <- expandOrPatterns p
        pure (PAs a ll q1 :| [PAs a ll q | q <- qs])
      p ->
        pure (NonEmpty.singleton p)
