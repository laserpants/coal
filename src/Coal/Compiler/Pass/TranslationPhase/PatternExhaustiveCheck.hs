{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TranslationPhase.PatternExhaustiveCheck (passPatternExhaustiveCheck) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Journal (listenErrors, tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.PatternMatching.AnomalyDetection (exhaustive, translatePattern)
import Coal.Compiler.Stack
import Coal.Language (IndexedType, Kind (..))
import Coal.Language.Expression (Clause (..), Expression (..))
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Expression.Choice (Choice (..), Guard (..))
import Coal.Language.Module
import Coal.Language.Pattern (Pattern (..))
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Extras (Name)

passPatternExhaustiveCheck :: (Monad m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passPatternExhaustiveCheck = Pass{runPass = patternExhaustiveCheckM}

patternExhaustiveCheckM :: (Monad m) => Module Metadata k t -> CompilerT Metadata m (Module Metadata k t)
patternExhaustiveCheckM m = do
  (m', es) <-
    listenErrors $
      overModuleDefinitionsM (traverse (patternExhaustiveCheck (modulePathName m))) m
  unless (null es) (throwError PatternAnomaly)
  pure m'

class PatternExhaustiveCheckContext c where
  patternExhaustiveCheck :: (Monad m) => Name -> c -> CompilerT Metadata m c

instance PatternExhaustiveCheckContext (Definition Metadata k t) where
  patternExhaustiveCheck name =
    \case
      DFunction loc n f ws ->
        DFunction loc n <$> traverse (patternExhaustiveCheck name) f <*> traverse (patternExhaustiveCheck name) ws
      DConstant loc n c ws ->
        DConstant loc n <$> patternExhaustiveCheck name c <*> traverse (patternExhaustiveCheck name) ws
      DInstance loc n d ->
        DInstance loc n <$> patternExhaustiveCheck name d
      d ->
        pure d

instance PatternExhaustiveCheckContext (InstanceDefinition Definition Metadata k t) where
  patternExhaustiveCheck name =
    \case
      InstanceDefinition ts t ds ->
        InstanceDefinition ts t <$> traverse (patternExhaustiveCheck name) ds

instance PatternExhaustiveCheckContext (FunctionDefinition Metadata t) where
  patternExhaustiveCheck name =
    \case
      FunctionDefinition loc w1 w2 ps e1 ->
        FunctionDefinition loc w1 w2 ps <$> patternExhaustiveCheck name e1

instance PatternExhaustiveCheckContext (ConstantDefinition Metadata t) where
  patternExhaustiveCheck name =
    \case
      ConstantDefinition loc w1 w2 e1 ->
        ConstantDefinition loc w1 w2 <$> patternExhaustiveCheck name e1

instance PatternExhaustiveCheckContext (Binding Expression Metadata t) where
  patternExhaustiveCheck name =
    \case
      BPattern a p e ->
        BPattern a p <$> patternExhaustiveCheck name e
      BFunction a n ps e ->
        BFunction a n ps <$> patternExhaustiveCheck name e

instance PatternExhaustiveCheckContext (Choice Expression Metadata t) where
  patternExhaustiveCheck name =
    \case
      CPlain a gs e ->
        CPlain a
          <$> traverse (patternExhaustiveCheck name) gs
          <*> patternExhaustiveCheck name e

instance PatternExhaustiveCheckContext (Guard Expression Metadata t) where
  patternExhaustiveCheck name =
    \case
      CGuard e ->
        CGuard <$> patternExhaustiveCheck name e

instance PatternExhaustiveCheckContext (Clause Metadata t) where
  patternExhaustiveCheck name =
    \case
      EClause a p cs ->
        EClause a p <$> traverse (patternExhaustiveCheck name) cs

instance PatternExhaustiveCheckContext (Expression Metadata t) where
  patternExhaustiveCheck name =
    \case
      EAnnotation a t e ->
        EAnnotation a t <$> patternExhaustiveCheck name e
      EApplication a t e es ->
        EApplication a t
          <$> patternExhaustiveCheck name e
          <*> traverse (patternExhaustiveCheck name) es
      ELambda a ps e ->
        ELambda a ps <$> patternExhaustiveCheck name e
      ELet a gs e1 ->
        ELet a
          <$> traverse (patternExhaustiveCheck name) gs
          <*> patternExhaustiveCheck name e1
      ERecursiveLet a p e1 e2 ->
        ERecursiveLet a p
          <$> patternExhaustiveCheck name e1
          <*> patternExhaustiveCheck name e2
      var@EVariable{} ->
        pure var
      con@EConstructor{} ->
        pure con
      lit@ELiteral{} ->
        pure lit
      EIf a t e1 e2 e3 ->
        EIf a t
          <$> patternExhaustiveCheck name e1
          <*> patternExhaustiveCheck name e2
          <*> patternExhaustiveCheck name e3
      op@EUnaryOperator{} ->
        pure op
      op@EBinaryOperator{} ->
        pure op
      ERecord a t d me ->
        ERecord a t
          <$> traverse (patternExhaustiveCheck name) d
          <*> traverse (patternExhaustiveCheck name) me
      EListCons a t e1 e2 ->
        EListCons a t
          <$> patternExhaustiveCheck name e1
          <*> patternExhaustiveCheck name e2
      EListLiteral a t es ->
        EListLiteral a t
          <$> traverse (patternExhaustiveCheck name) es
      ETuple a t es ->
        ETuple a t <$> traverse (patternExhaustiveCheck name) es
      EMatch a t e cs ->
        EMatch a t
          <$> patternExhaustiveCheck name e
          <*> (checkExhaustive name a cs >> traverse (patternExhaustiveCheck name) cs)
      ELambdaMatch a t cs ->
        ELambdaMatch a t
          <$> (checkExhaustive name a cs >> traverse (patternExhaustiveCheck name) cs)
      ECompiledMatch{} ->
        error "Implementation error"
      ESelect a ll e ->
        ESelect a ll <$> patternExhaustiveCheck name e
      EFocus a n ll1 ll2 e1 e2 ->
        EFocus a n ll1 ll2
          <$> patternExhaustiveCheck name e1
          <*> patternExhaustiveCheck name e2
      EFFICall a t ll es e ->
        EFFICall a t ll
          <$> traverse (patternExhaustiveCheck name) es
          <*> patternExhaustiveCheck name e
      e@ETraitDictionary{} ->
        pure e
      e@EDoBlock{} ->
        pure e
      e@EFold{} ->
        pure e

checkExhaustive :: (Monad m) => Name -> Metadata -> NonEmpty (Clause Metadata t) -> CompilerT Metadata m (NonEmpty (Clause Metadata t))
checkExhaustive name loc cs = do
  isExhaustive <- exhaustive patterns
  unless isExhaustive $ do
    tellErrors [NonExhaustivePatterns (ErrorLocation name loc)]
  pure cs
 where
  patterns = NonEmpty.toList (translatePattern . clausePattern <$> cs)

clausePattern :: Clause a t -> Pattern a t
clausePattern =
  \case
    EClause _ p _ ->
      p
