{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.Constraint.GenerationSpec where

import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.Language.Constructor (Constructor (..))
import Coal.TypeSystem.Constraint
import Coal.TypeSystem.Constraint.Generation
import Coal.TypeSystem.Constraint.Generation.InferenceRule
import Data.Either (lefts, rights)

import qualified Coal.Common.Environment as Environment

collectConstraintsSpecAll = do
  print collectConstraintsSpec1
  print collectConstraintsSpec2
  print collectConstraintsSpec3
  print collectConstraintsSpec4
  print collectConstraintsSpec5
  print collectConstraintsSpec6

fixture1 :: Expression () IndexedType
fixture1 =
  EConstructor () (Label (TConstructor KType "Color") "Blue")

collectConstraintsSpec1 = ENoDataConstructor () "Blue" `elem` lefts outs
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

collectConstraintsSpec2 = null (lefts outs)
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

collectConstraintsSpec3 = ENoDataConstructor () "Blue" `elem` lefts outs
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

collectConstraintsSpec4 = null (lefts outs)
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

constraint1 =
  Explicit
    (RuleDataConstructor () "Blue" (TVariable (TypeIndex KType 0)) (Forall mempty [] (TConstructor KType "Color")))
    (TVariable (TypeIndex KType 0))
    (Forall mempty [] (TConstructor KType "Color"))

collectConstraintsSpec5 = constraint1 `elem` rights outs
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

collectConstraintsSpec6 = 1 == freshIdIn fixture2
