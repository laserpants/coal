{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.Constraint.Generation.EAnnotationConstraintsSpec where

import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.TypeSystem.Constraint
import Coal.TypeSystem.Constraint.Generation
import Coal.TypeSystem.Constraint.Generation.InferenceRule
import Data.Either (lefts, rights)

collectEAnnotationConstraintsSpecAll = do
  print collectEAnnotationConstraintsSpec1
  print collectEAnnotationConstraintsSpec2
  print collectEAnnotationConstraintsSpec3
  print collectEAnnotationConstraintsSpec4

fixture1 :: Expression () IndexedType
fixture1 =
  EAnnotation () (TIntrinsic IInt32) (ELiteral () (LInt32 1))

collectEAnnotationConstraintsSpec1 :: Bool
collectEAnnotationConstraintsSpec1 = null ms && null (lefts outs)
 where
  (ms, outs) = evalConstraintsGenStack (freshIdIn expr) ctx (emitConstraints expr)
  expr = fixture1
  ctx =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextDataConstructorEnv = mempty
      , constraintsGenContextCodataAccessorEnv = mempty
      , constraintsGenContextTypeConstructorEnv = mempty
      , constraintsGenContextTopLevelFoldEnv = mempty
      }

constraint1 :: Constraint (InferenceRule Kind ()) TypeIndex Kind IndexedType
constraint1 = Equality (RuleAnnotation () (TIntrinsic IInt32) (TIntrinsic IInt32)) [TIntrinsic IInt32, TIntrinsic IInt32]

collectEAnnotationConstraintsSpec2 :: Bool
collectEAnnotationConstraintsSpec2 = null ms && constraint1 `elem` rights outs
 where
  (ms, outs) = evalConstraintsGenStack (freshIdIn expr) ctx (emitConstraints expr)
  expr = fixture1
  ctx =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextDataConstructorEnv = mempty
      , constraintsGenContextCodataAccessorEnv = mempty
      , constraintsGenContextTypeConstructorEnv = mempty
      , constraintsGenContextTopLevelFoldEnv = mempty
      }

fixture2 :: Expression () IndexedType
fixture2 =
  EAnnotation () (TIntrinsic IBool) (ELiteral () (LInt32 1))

constraint2 :: Constraint (InferenceRule Kind ()) TypeIndex Kind IndexedType
constraint2 = Equality (RuleAnnotation () (TIntrinsic IInt32) (TIntrinsic IBool)) [TIntrinsic IInt32, TIntrinsic IBool]

collectEAnnotationConstraintsSpec3 :: Bool
collectEAnnotationConstraintsSpec3 = null ms && constraint2 `elem` rights outs
 where
  (ms, outs) = evalConstraintsGenStack (freshIdIn expr) ctx (emitConstraints expr)
  expr = fixture2
  ctx =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextDataConstructorEnv = mempty
      , constraintsGenContextCodataAccessorEnv = mempty
      , constraintsGenContextTypeConstructorEnv = mempty
      , constraintsGenContextTopLevelFoldEnv = mempty
      }

fixture3 :: Expression () IndexedType
fixture3 =
  EAnnotation () (TVariable (Parameter () "a")) (ELiteral () (LInt32 1))

collectEAnnotationConstraintsSpec4 :: Bool
collectEAnnotationConstraintsSpec4 = null ms && null (lefts outs)
 where
  (ms, outs) = evalConstraintsGenStack (freshIdIn expr) ctx (emitConstraints expr)
  expr = fixture3
  ctx =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextDataConstructorEnv = mempty
      , constraintsGenContextCodataAccessorEnv = mempty
      , constraintsGenContextTypeConstructorEnv = mempty
      , constraintsGenContextTopLevelFoldEnv = mempty
      }
