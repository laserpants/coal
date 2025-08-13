{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.Constraint.Generation.EConstructorConstraintsSpec where

import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.Language.Constructor (Constructor (..))
import Coal.TypeSystem.Constraint
import Coal.TypeSystem.Constraint.Generation
import Coal.TypeSystem.Constraint.Generation.InferenceRule
import Data.Either (lefts, rights)

import qualified Coal.Common.Environment as Environment

collectEConstructorConstraintsSpecAll = do
  print collectEConstructorConstraintsSpec1
  print collectEConstructorConstraintsSpec2
  print collectEConstructorConstraintsSpec3
  print collectEConstructorConstraintsSpec4
  print collectEConstructorConstraintsSpec5
  print collectEConstructorConstraintsSpec6

fixture1 :: Expression () IndexedType
fixture1 =
  EConstructor () (Label (TConstructor KType "Color") "Blue")

collectEConstructorConstraintsSpec1 :: Bool
collectEConstructorConstraintsSpec1 = ENoDataConstructor () "Blue" `elem` lefts outs
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

collectEConstructorConstraintsSpec2 :: Bool
collectEConstructorConstraintsSpec2 = null (lefts outs)
 where
  (_, outs) = evalConstraintsGenStack (freshIdIn expr) ctx (collectConstraints expr)
  expr = fixture1
  ctx =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextDataConstructorEnv =
          Environment.fromList
            [
              ( "Blue"
              , Constructor "Blue" 0 (Forall mempty [] (TConstructor KType "Color"))
              )
            ]
      , constraintsGenContextCodataAccessorEnv = mempty
      , constraintsGenContextTypeConstructorEnv = mempty
      }

fixture2 :: Expression () IndexedType
fixture2 =
  EConstructor () (Label (TVariable (TypeIndex KType 0)) "Blue")

collectEConstructorConstraintsSpec3 :: Bool
collectEConstructorConstraintsSpec3 = ENoDataConstructor () "Blue" `elem` lefts outs
 where
  expr = fixture2
  (_, outs) = evalConstraintsGenStack (freshIdIn expr) ctx (collectConstraints expr)
  ctx =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextDataConstructorEnv = mempty
      , constraintsGenContextCodataAccessorEnv = mempty
      , constraintsGenContextTypeConstructorEnv = mempty
      }

collectEConstructorConstraintsSpec4 :: Bool
collectEConstructorConstraintsSpec4 = null (lefts outs)
 where
  (_, outs) = evalConstraintsGenStack (freshIdIn expr) ctx (collectConstraints expr)
  expr = fixture2
  ctx =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextDataConstructorEnv =
          Environment.fromList
            [
              ( "Blue"
              , Constructor "Blue" 0 (Forall mempty [] (TConstructor KType "Color"))
              )
            ]
      , constraintsGenContextCodataAccessorEnv = mempty
      , constraintsGenContextTypeConstructorEnv = mempty
      }

constraint1 :: Constraint (InferenceRule Kind ()) TypeIndex Kind IndexedType
constraint1 =
  Explicit
    (RuleDataConstructor () "Blue" (TVariable (TypeIndex KType 0)) (Forall mempty [] (TConstructor KType "Color")))
    (TVariable (TypeIndex KType 0))
    (Forall mempty [] (TConstructor KType "Color"))

collectEConstructorConstraintsSpec5 :: Bool
collectEConstructorConstraintsSpec5 = constraint1 `elem` rights outs
 where
  (_, outs) = evalConstraintsGenStack (freshIdIn expr) ctx (collectConstraints expr)
  expr = fixture2
  ctx =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextDataConstructorEnv =
          Environment.fromList
            [
              ( "Blue"
              , Constructor "Blue" 0 (Forall mempty [] (TConstructor KType "Color"))
              )
            ]
      , constraintsGenContextCodataAccessorEnv = mempty
      , constraintsGenContextTypeConstructorEnv = mempty
      }

collectEConstructorConstraintsSpec6 :: Bool
collectEConstructorConstraintsSpec6 = 1 == freshIdIn fixture2
