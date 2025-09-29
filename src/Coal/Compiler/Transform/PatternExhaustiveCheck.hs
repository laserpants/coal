{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Transform.PatternExhaustiveCheck (PatternExhaustiveCheckContext (..), patternExhaustiveCheckM) where

import Coal.Compiler.Error (CompilerError (..), ErrorLocation (..))
import Coal.Compiler.Journal
import Coal.Compiler.PatternAnomalies
import Coal.Compiler.Stack
import Coal.Language.Expression
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Expression.Choice (Choice (..), Guard (..))
import Coal.Language.Module
import Coal.Language.Pattern (Pattern (..))
import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Debug.Trace
import Extra (Name)

patternExhaustiveCheckM :: (Monad m) => Module a k t -> CompilerT a m (Module a k t)
patternExhaustiveCheckM m = overModuleDefinitionsM (traverse (patternExhaustiveCheck (modulePathName m))) m

class PatternExhaustiveCheckContext c where
  patternExhaustiveCheck :: (Monad m) => Name -> c -> CompilerT a m c

instance PatternExhaustiveCheckContext (Definition a k t) where
  patternExhaustiveCheck name =
    \case
      DFunction loc n f ws ->
        DFunction loc n <$> patternExhaustiveCheck name f <*> traverse (patternExhaustiveCheck name) ws
      DConstant loc n c ws ->
        DConstant loc n <$> patternExhaustiveCheck name c <*> traverse (patternExhaustiveCheck name) ws
      DFold loc n d ->
        DFold loc n <$> patternExhaustiveCheck name d
      DUnfold loc n d ->
        DUnfold loc n <$> patternExhaustiveCheck name d
      DInstance loc n d ->
        DInstance loc n <$> patternExhaustiveCheck name d
      d ->
        pure d

instance PatternExhaustiveCheckContext (InstanceDef Definition a k t) where
  patternExhaustiveCheck name =
    \case
      InstanceDef ts t ds ->
        InstanceDef ts t <$> traverse (patternExhaustiveCheck name) ds

instance PatternExhaustiveCheckContext (FoldDef a t) where
  patternExhaustiveCheck name =
    \case
      FoldDef w t e ->
        FoldDef w t <$> traverse (patternExhaustiveCheck name) e

instance PatternExhaustiveCheckContext (UnfoldDef a t) where
  patternExhaustiveCheck name =
    \case
      UnfoldDef w t ps e ->
        UnfoldDef w t ps <$> traverse (patternExhaustiveCheck name) e

instance PatternExhaustiveCheckContext (FunctionDef a t) where
  patternExhaustiveCheck name =
    \case
      FunctionDef loc w1 w2 ps e1 ->
        FunctionDef loc w1 w2 ps <$> patternExhaustiveCheck name e1

instance PatternExhaustiveCheckContext (ConstantDef a t) where
  patternExhaustiveCheck name =
    \case
      ConstantDef loc w1 w2 e1 ->
        ConstantDef loc w1 w2 <$> patternExhaustiveCheck name e1

instance PatternExhaustiveCheckContext (Binding Expression a t) where
  patternExhaustiveCheck name =
    \case
      BPattern a p e ->
        BPattern a p <$> patternExhaustiveCheck name e
      BFunction{} ->
        error "TODO"

instance PatternExhaustiveCheckContext (Choice Expression a t) where
  patternExhaustiveCheck name =
    \case
      CPlain a gs e ->
        CPlain a
          <$> traverse (patternExhaustiveCheck name) gs
          <*> patternExhaustiveCheck name e
      CLambda{} ->
        error "Not implemented"

instance PatternExhaustiveCheckContext (Guard Expression a t) where
  patternExhaustiveCheck name =
    \case
      CGuard e ->
        CGuard <$> patternExhaustiveCheck name e

instance PatternExhaustiveCheckContext (Clause a t) where
  patternExhaustiveCheck name =
    \case
      EClause a p cs ->
        EClause a p <$> traverse (patternExhaustiveCheck name) cs

instance PatternExhaustiveCheckContext (Expression a t) where
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
      ELambdaMatch a t cs me ->
        ELambdaMatch a t
          <$> (checkExhaustive name a cs >> traverse (patternExhaustiveCheck name) cs)
          <*> traverse (patternExhaustiveCheck name) me
      ECompiledMatch{} ->
        error "Implementation error"
      EFold a t es cs me ->
        EFold a t
          <$> traverse (patternExhaustiveCheck name) es
          <*> (checkExhaustive name a cs >> traverse (patternExhaustiveCheck name) cs)
          <*> traverse (patternExhaustiveCheck name) me
      ESelect a ll e ->
        ESelect a ll <$> patternExhaustiveCheck name e
      ECodataSelect a ll e me ->
        ECodataSelect a ll
          <$> patternExhaustiveCheck name e
          <*> traverse (patternExhaustiveCheck name) me
      ECodataRecord a t d ->
        ECodataRecord a t
          <$> traverse (patternExhaustiveCheck name) d
      EFocus n ll1 ll2 e1 e2 ->
        EFocus n ll1 ll2
          <$> patternExhaustiveCheck name e1
          <*> patternExhaustiveCheck name e2
      trait@ETraitDictionary{} ->
        pure trait

checkExhaustive :: (Monad m) => Name -> a -> NonEmpty (Clause a t) -> CompilerT b m (NonEmpty (Clause a t))
checkExhaustive name loc cs = do
  isExhaustive <- exhaustive patterns
  unless isExhaustive $ do
    -- tellErrors [NonExhaustivePatterns (ErrorLocation name loc)]
    error ("NonExhaustivePatterns" <> show patterns)
  pure cs
 where
  patterns = NonEmpty.toList (translatePattern . clausePattern <$> cs)

clausePattern :: Clause a t -> Pattern a t
clausePattern =
  \case
    EClause _ p _ ->
      p
