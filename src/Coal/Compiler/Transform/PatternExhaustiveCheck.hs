{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Transform.PatternExhaustiveCheck (PatternExhaustiveCheckContext (..)) where

import Coal.Compiler.Stack
import Coal.Language.Expression
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Module
import Coal.Language.Pattern (Pattern (..))
import Data.List.NonEmpty (NonEmpty)

class PatternExhaustiveCheckContext c where
  patternExhaustiveCheck :: (Monad m) => c -> CompilerT a m c

instance PatternExhaustiveCheckContext (Module a k t) where
  patternExhaustiveCheck = overModuleDefinitionsM (traverse patternExhaustiveCheck)

instance PatternExhaustiveCheckContext (Definition a k t) where
  patternExhaustiveCheck =
    \case
      DFunction loc name f ws ->
        DFunction loc name <$> patternExhaustiveCheck f <*> traverse patternExhaustiveCheck ws
      DConstant loc name c ws ->
        DConstant loc name <$> patternExhaustiveCheck c <*> traverse patternExhaustiveCheck ws
      DFold loc name d ->
        DFold loc name <$> patternExhaustiveCheck d
      DUnfold loc name d ->
        DUnfold loc name <$> patternExhaustiveCheck d
      DInstance loc name d ->
        DInstance loc name <$> patternExhaustiveCheck d
      d ->
        pure d

instance PatternExhaustiveCheckContext (InstanceDef Definition a k t) where
  patternExhaustiveCheck =
    \case
      InstanceDef ts t ds ->
        InstanceDef ts t <$> traverse patternExhaustiveCheck ds

instance PatternExhaustiveCheckContext (FoldDef a t) where
  patternExhaustiveCheck =
    \case
      FoldDef w t e ->
        FoldDef w t <$> traverse patternExhaustiveCheck e

instance PatternExhaustiveCheckContext (UnfoldDef a t) where
  patternExhaustiveCheck =
    \case
      UnfoldDef w t ps e ->
        UnfoldDef w t ps <$> traverse patternExhaustiveCheck e

instance PatternExhaustiveCheckContext (FunctionDef a t) where
  patternExhaustiveCheck =
    \case
      FunctionDef loc w1 w2 ps e1 ->
        FunctionDef loc w1 w2 ps <$> patternExhaustiveCheck e1

instance PatternExhaustiveCheckContext (ConstantDef a t) where
  patternExhaustiveCheck =
    \case
      ConstantDef loc w1 w2 e1 ->
        ConstantDef loc w1 w2 <$> patternExhaustiveCheck e1

instance PatternExhaustiveCheckContext (Binding Expression a t) where
  patternExhaustiveCheck =
    \case
      BPattern a p e ->
        BPattern a p <$> patternExhaustiveCheck e
      BFunction{} ->
        error "TODO"

instance PatternExhaustiveCheckContext (Expression a t) where
  patternExhaustiveCheck =
    \case
      EAnnotation a t e ->
        EAnnotation a t <$> patternExhaustiveCheck e
      EApplication a t e es ->
        EApplication a t
          <$> patternExhaustiveCheck e
          <*> traverse patternExhaustiveCheck es
      ELambda a ps e ->
        ELambda a ps <$> patternExhaustiveCheck e
      ELet a gs e1 ->
        ELet a
          <$> traverse patternExhaustiveCheck gs
          <*> patternExhaustiveCheck e1
      ERecursiveLet a p e1 e2 ->
        ERecursiveLet a p
          <$> patternExhaustiveCheck e1
          <*> patternExhaustiveCheck e2
      var@EVariable{} ->
        pure var
      con@EConstructor{} ->
        pure con
      lit@ELiteral{} ->
        pure lit
      EIf a t e1 e2 e3 ->
        EIf a t
          <$> patternExhaustiveCheck e1
          <*> patternExhaustiveCheck e2
          <*> patternExhaustiveCheck e3
      op@EUnaryOperator{} ->
        pure op
      op@EBinaryOperator{} ->
        pure op
      ERecord a t d me ->
        ERecord a t
          <$> traverse patternExhaustiveCheck d
          <*> traverse patternExhaustiveCheck me
      EListCons a t e1 e2 ->
        EListCons a t
          <$> patternExhaustiveCheck e1
          <*> patternExhaustiveCheck e2
      EListLiteral a t es ->
        EListLiteral a t
          <$> traverse patternExhaustiveCheck es
      ETuple a t es ->
        ETuple a t <$> traverse patternExhaustiveCheck es
      EMatch a t e cs ->
        EMatch a t
          <$> patternExhaustiveCheck e
          <*> baz cs
      ELambdaMatch a t cs me ->
        ELambdaMatch a t
          <$> baz cs
          <*> traverse patternExhaustiveCheck me
      ECompiledMatch{} ->
        error "Implementation error"
      EFold a t es cs me ->
        EFold a t
          <$> traverse patternExhaustiveCheck es
          <*> baz cs
          <*> traverse patternExhaustiveCheck me
      ESelect a ll e ->
        ESelect a ll <$> patternExhaustiveCheck e
      ECodataSelect a ll e me ->
        ECodataSelect a ll
          <$> patternExhaustiveCheck e
          <*> traverse patternExhaustiveCheck me
      ECodataRecord a t d ->
        ECodataRecord a t
          <$> traverse patternExhaustiveCheck d
      EFocus name ll1 ll2 e1 e2 ->
        EFocus name ll1 ll2
          <$> patternExhaustiveCheck e1
          <*> patternExhaustiveCheck e2
      trait@ETraitDictionary{} ->
        pure trait

-- TODO
baz :: NonEmpty (Clause a t) -> CompilerT b m (NonEmpty (Clause a t))
baz cs = undefined
 where
  patterns = clausePattern <$> cs

clausePattern :: Clause a t -> Pattern a t
clausePattern =
  \case
    EClause _ p _ ->
      p
