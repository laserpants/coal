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
import Coal.ProtoCompiler.ProtoStack
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Extras (Name, traverse_)

passPatternExhaustiveCheck :: (Monad m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passPatternExhaustiveCheck = Pass{runPass = bork}

bork :: (Monad m) => Module Metadata k t -> CompilerT Metadata (ProtoCompilerT m Metadata) (Module Metadata k t)
bork m = do
  patternExhaustiveCheckM m
  return m

patternExhaustiveCheckM :: (Monad m) => Module Metadata k t -> CompilerT Metadata (ProtoCompilerT m Metadata) ()
patternExhaustiveCheckM m = do
  (_, es) <-
    listenErrors $
      traverse_ (patternExhaustiveCheck (modulePathName m)) (moduleDefinitions m)
  unless (null es) (throwError PatternAnomaly)

class PatternExhaustiveCheckContext c where
  patternExhaustiveCheck :: (Monad m) => Name -> c -> CompilerT Metadata (ProtoCompilerT m Metadata) ()

instance PatternExhaustiveCheckContext (Definition Metadata k t) where
  patternExhaustiveCheck name =
    \case
      DFunction loc n f ws -> do
        traverse_ (patternExhaustiveCheck name) f
        traverse_ (patternExhaustiveCheck name) ws
      DConstant loc n c ws -> do
        patternExhaustiveCheck name c
        traverse_ (patternExhaustiveCheck name) ws
      DInstance loc n d ->
        patternExhaustiveCheck name d
      _ ->
        pure ()

instance PatternExhaustiveCheckContext (InstanceDefinition Definition Metadata k t) where
  patternExhaustiveCheck name =
    \case
      InstanceDefinition ts t ds ->
        traverse_ (patternExhaustiveCheck name) ds
--    \case
--      InstanceDefinition ts t ds ->
--        InstanceDefinition ts t <$> traverse (patternExhaustiveCheck name) ds

instance PatternExhaustiveCheckContext (FunctionDefinition Metadata t) where
  patternExhaustiveCheck name =
    \case
      FunctionDefinition loc w1 w2 ps e1 ->
        patternExhaustiveCheck name e1
--        FunctionDefinition loc w1 w2 ps <$> patternExhaustiveCheck name e1

instance PatternExhaustiveCheckContext (ConstantDefinition Metadata t) where
  patternExhaustiveCheck name =
    \case
      ConstantDefinition loc w1 w2 e1 ->
        patternExhaustiveCheck name e1
--        ConstantDefinition loc w1 w2 <$> patternExhaustiveCheck name e1

instance PatternExhaustiveCheckContext (Binding Expression Metadata () t) where
  patternExhaustiveCheck name =
    \case
      BPattern a p e ->
        patternExhaustiveCheck name e
      BFunction a n ps e ->
        patternExhaustiveCheck name e
--    \case
--      BPattern a p e ->
--        BPattern a p <$> patternExhaustiveCheck name e
--      BFunction a n ps e ->
--        BFunction a n ps <$> patternExhaustiveCheck name e

instance PatternExhaustiveCheckContext (Choice Expression Metadata () t) where
  patternExhaustiveCheck name =
    \case
      CPlain a gs e -> do
        traverse_ (patternExhaustiveCheck name) gs
        patternExhaustiveCheck name e
--    \case
--      CPlain a gs e ->
--        CPlain a
--          <$> traverse (patternExhaustiveCheck name) gs
--          <*> patternExhaustiveCheck name e

instance PatternExhaustiveCheckContext (Guard Expression Metadata () t) where
  patternExhaustiveCheck name =
    \case
      CGuard e ->
        patternExhaustiveCheck name e
--    \case
--      CGuard e ->
--        CGuard <$> patternExhaustiveCheck name e

instance PatternExhaustiveCheckContext (Clause Metadata () t) where
  patternExhaustiveCheck name =
    \case
      EClause a p cs ->
        traverse_ (patternExhaustiveCheck name) cs
--    \case
--      EClause a p cs ->
--        EClause a p <$> traverse (patternExhaustiveCheck name) cs

instance PatternExhaustiveCheckContext (Expression Metadata () t) where
  patternExhaustiveCheck name =
    \case
      EAnnotation a t e ->
        patternExhaustiveCheck name e
--        EAnnotation a t <$> patternExhaustiveCheck name e
      EApplication a t e es -> do
          patternExhaustiveCheck name e
          traverse_ (patternExhaustiveCheck name) es
--      EApplication a t e es ->
--        EApplication a t
--          <$> patternExhaustiveCheck name e
--          <*> traverse (patternExhaustiveCheck name) es
      ELambda a ps e ->
        patternExhaustiveCheck name e
--      ELambda a ps e ->
--        ELambda a ps <$> patternExhaustiveCheck name e
      ELet a gs e1 -> do
          traverse_ (patternExhaustiveCheck name) gs
          patternExhaustiveCheck name e1
--      ELet a gs e1 ->
--        ELet a
--          <$> traverse (patternExhaustiveCheck name) gs
--          <*> patternExhaustiveCheck name e1
      ERecursiveLet a p e1 e2 -> do
          patternExhaustiveCheck name e1
          patternExhaustiveCheck name e2
--      ERecursiveLet a p e1 e2 ->
--        ERecursiveLet a p
--          <$> patternExhaustiveCheck name e1
--          <*> patternExhaustiveCheck name e2
--      var@EVariable{} ->
--        pure var
--      con@EConstructor{} ->
--        pure con
--      lit@ELiteral{} ->
--        pure lit
      EIf a t e1 e2 e3 -> do
          patternExhaustiveCheck name e1
          patternExhaustiveCheck name e2
          patternExhaustiveCheck name e3
--      op@EOperator{} ->
--        pure op
--      ERecord a t d me ->
--        ERecord a t
--          <$> traverse (patternExhaustiveCheck name) d
--          <*> traverse (patternExhaustiveCheck name) me
      ERecord a t d me -> do
          traverse_ (patternExhaustiveCheck name) d
          traverse_ (patternExhaustiveCheck name) me
--      EListCons a t e1 e2 ->
--        EListCons a t
--          <$> patternExhaustiveCheck name e1
--          <*> patternExhaustiveCheck name e2
      EListCons a t e1 e2 -> do
          patternExhaustiveCheck name e1
          patternExhaustiveCheck name e2
--      EListLiteral a t es ->
--        EListLiteral a t
--          <$> traverse (patternExhaustiveCheck name) es
      EListLiteral a t es ->
          traverse_ (patternExhaustiveCheck name) es
--      ETuple a t es ->
--        ETuple a t <$> traverse (patternExhaustiveCheck name) es
      ETuple a t es ->
        traverse_ (patternExhaustiveCheck name) es
--      EMatch a t e cs ->
--        EMatch a t
--          <$> patternExhaustiveCheck name e
--          <*> (checkExhaustive name a cs >> traverse (patternExhaustiveCheck name) cs)
      EMatch a t e cs -> do
        patternExhaustiveCheck name e
        checkExhaustive name a cs
        traverse_ (patternExhaustiveCheck name) cs
--        EMatch a t
--          <$> patternExhaustiveCheck name e
--          <*> (checkExhaustive name a cs >> traverse (patternExhaustiveCheck name) cs)
--      ELambdaMatch a t cs ->
--        ELambdaMatch a t
--          <$> (checkExhaustive name a cs >> traverse (patternExhaustiveCheck name) cs)
      ELambdaMatch a t cs -> do
          checkExhaustive name a cs
          traverse_ (patternExhaustiveCheck name) cs
--      ECompiledMatch{} ->
--        error "Implementation error"
--      ESelect a ll e ->
--        ESelect a ll <$> patternExhaustiveCheck name e
      ESelect a ll e ->
        patternExhaustiveCheck name e
--      EFocus a n ll1 ll2 e1 e2 ->
--        EFocus a n ll1 ll2
--          <$> patternExhaustiveCheck name e1
--          <*> patternExhaustiveCheck name e2
      EFocus a n ll1 ll2 e1 e2 -> do
          patternExhaustiveCheck name e1
          patternExhaustiveCheck name e2
--      EFFICall a t ll es e ->
--        EFFICall a t ll
--          <$> traverse (patternExhaustiveCheck name) es
--          <*> patternExhaustiveCheck name e
      EFFICall a t ll es e -> do
          traverse_ (patternExhaustiveCheck name) es
          patternExhaustiveCheck name e
      _ ->
        pure ()
--      e@ETraitInstance{} ->
--        pure e
--      e@EDoBlock{} ->
--        pure e
--      e@EFold{} ->
--        pure e

checkExhaustive :: (Monad m) => Name -> Metadata -> NonEmpty (Clause Metadata () t) -> CompilerT Metadata (ProtoCompilerT m Metadata) ()
checkExhaustive name loc cs = do
  isExhaustive <- exhaustive patterns
  unless isExhaustive $ do
    tellErrors [NonExhaustivePatterns (ErrorLocation name loc)]
 where
  patterns = NonEmpty.toList (translatePattern . clausePattern <$> cs)
