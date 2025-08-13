{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.Constraint.Generation.EAnnotationConstraintsSpec where

import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.TypeSystem.Constraint
import Coal.TypeSystem.Constraint.Generation
import Coal.TypeSystem.Constraint.Generation.InferenceRule
import Data.Either (lefts, rights)

fixture1 :: Expression () IndexedType
fixture1 =
  EAnnotation () (TIntrinsic IInt32) (ELiteral () (LInt32 1))

collectEAnnotationConstraintsSpec1 :: Bool
collectEAnnotationConstraintsSpec1 = null (lefts outs)
 where
  (_, outs) = evalConstraintsGenStack (freshIdIn expr) ctx (collectConstraints expr)
  expr = fixture1
  ctx =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextDataConstructorEnv = mempty
      , constraintsGenContextCodataAccessorEnv = mempty
      , constraintsGenContextTypeConstructorEnv = mempty
      }

constraint1 :: Constraint (InferenceRule Kind ()) TypeIndex Kind IndexedType
constraint1 = Equality (RuleAnnotation () (TIntrinsic IInt32) (TIntrinsic IInt32)) [TIntrinsic IInt32, TIntrinsic IInt32]

collectEAnnotationConstraintsSpec2 :: Bool
collectEAnnotationConstraintsSpec2 = constraint1 `elem` rights outs
 where
  (_, outs) = evalConstraintsGenStack (freshIdIn expr) ctx (collectConstraints expr)
  expr = fixture1
  ctx =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextDataConstructorEnv = mempty
      , constraintsGenContextCodataAccessorEnv = mempty
      , constraintsGenContextTypeConstructorEnv = mempty
      }

