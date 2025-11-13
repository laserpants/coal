{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.Constraint.Generation.EConstructorConstraintsSpec where

import qualified Coal.Common.Environment as Environment
import Coal.Common.Label (Label (..))
import Coal.Compiler.Module.Bundle (emptyModuleBundle)
import Coal.Language
import Coal.TypeSystem.Constraint
import Coal.TypeSystem.Constraint.Generation
import Coal.TypeSystem.Constraint.Generation.InferenceRule
import Data.Either (lefts, rights)

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
collectEConstructorConstraintsSpec1 = null ms && ENoDataConstructor () "Blue" `elem` lefts outs
 where
  (ms, outs) = evalConstraintsGenStack (freshIdIn expr) ctx (emitConstraints expr)
  expr = fixture1
  ctx =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextModules = emptyModuleBundle
      }

collectEConstructorConstraintsSpec2 :: Bool
collectEConstructorConstraintsSpec2 = null ms && null (lefts outs)
 where
  (ms, outs) = evalConstraintsGenStack (freshIdIn expr) ctx (emitConstraints expr)
  expr = fixture1
  ctx =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextModules = emptyModuleBundle
      }

fixture2 :: Expression () IndexedType
fixture2 =
  EConstructor () (Label (TVariable (TypeIndex KType 0)) "Blue")

collectEConstructorConstraintsSpec3 :: Bool
collectEConstructorConstraintsSpec3 = null ms && ENoDataConstructor () "Blue" `elem` lefts outs
 where
  expr = fixture2
  (ms, outs) = evalConstraintsGenStack (freshIdIn expr) ctx (emitConstraints expr)
  ctx =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextModules = emptyModuleBundle
      }

collectEConstructorConstraintsSpec4 :: Bool
collectEConstructorConstraintsSpec4 = null ms && null (lefts outs)
 where
  (ms, outs) = evalConstraintsGenStack (freshIdIn expr) ctx (emitConstraints expr)
  expr = fixture2
  ctx =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextModules = emptyModuleBundle
      }

constraint1 :: Constraint (InferenceRule Kind ()) TypeIndex Kind IndexedType
constraint1 =
  Explicit
    (RuleDataConstructor () "Blue" (TVariable (TypeIndex KType 0)) (Forall mempty [] (TConstructor KType "Color")))
    (TVariable (TypeIndex KType 0))
    (Forall mempty [] (TConstructor KType "Color"))

collectEConstructorConstraintsSpec5 :: Bool
collectEConstructorConstraintsSpec5 = null ms && constraint1 `elem` rights outs
 where
  (ms, outs) = evalConstraintsGenStack (freshIdIn expr) ctx (emitConstraints expr)
  expr = fixture2
  ctx =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextModules = emptyModuleBundle
      }

collectEConstructorConstraintsSpec6 :: Bool
collectEConstructorConstraintsSpec6 = 1 == freshIdIn fixture2
