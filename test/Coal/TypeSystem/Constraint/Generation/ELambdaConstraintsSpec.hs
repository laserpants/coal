{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.Constraint.Generation.ELambdaConstraintsSpec where

import Coal.Common.Label (Label (..))
import Coal.Common.List1 (List1, NonEmpty (..), fromList1, (<|))
import Coal.Language
import Coal.TypeSystem.Constraint
import Coal.TypeSystem.Constraint.Generation
import Coal.TypeSystem.Constraint.Generation.InferenceRule
import Data.Either (lefts, rights)

import qualified Coal.Common.Environment as Environment
import qualified Data.Set as Set

fixture1 :: Expression () IndexedType
fixture1 =
  ELambda
    ()
    (PVariable () (Label (TVariable (TypeIndex KType 0)) "x") :| [])
    (EVariable () (Label (TVariable (TypeIndex KType 1)) "x"))

constraint1 :: Constraint (InferenceRule Kind ()) TypeIndex Kind IndexedType
constraint1 =
  Equality
    InferenceRulePlaceholder
    [ TVariable (TypeIndex KType 1)
    , TVariable (TypeIndex KType 0)
    ]

collectELambdaConstraintsSpec1 :: Bool
collectELambdaConstraintsSpec1 = null ms && constraint1 `elem` rights outs
 where
  (ms, outs) = evalConstraintsGenStack (freshIdIn expr) ctx (collectConstraints expr)
  expr = fixture1
  ctx =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextDataConstructorEnv = mempty
      , constraintsGenContextCodataAccessorEnv = mempty
      , constraintsGenContextTypeConstructorEnv = mempty
      }

fixture2 :: Expression () IndexedType
fixture2 =
  ELambda
    ()
    (PVariable () (Label (TVariable (TypeIndex KType 0)) "x") :| [])
    (EVariable () (Label (TVariable (TypeIndex KType 1)) "y"))

collectELambdaConstraintsSpec2 :: Bool
collectELambdaConstraintsSpec2 = null outs
 where
  (ms, outs) = evalConstraintsGenStack (freshIdIn expr) ctx (collectConstraints expr)
  expr = fixture2
  ctx =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextDataConstructorEnv = mempty
      , constraintsGenContextCodataAccessorEnv = mempty
      , constraintsGenContextTypeConstructorEnv = mempty
      }

collectELambdaConstraintsSpec3 :: Bool
collectELambdaConstraintsSpec3 = 2 == freshIdIn fixture2

fixture3 :: Expression () IndexedType
fixture3 =
  -- fn(x) => let y = x in y
  ELambda
    ()
    (PVariable () (Label (TVariable (TypeIndex KType 0)) "x") :| [])
    ( ELet
        ()
        ( BPattern
            ()
            (PVariable () (Label (TVariable (TypeIndex KType 1)) "y"))
            (EVariable () (Label (TVariable (TypeIndex KType 2)) "x"))
            :| []
        )
        (EVariable () (Label (TVariable (TypeIndex KType 3)) "y"))
    )

muteConstraint :: Constraint c o k t -> Constraint () o k t
muteConstraint =
  \case
    Equality _ ts ->
      Equality () ts
    Implicit _ t1 t2 m ->
      Implicit () t1 t2 m
    Explicit _ t s ->
      Explicit () t s

constraint2 :: Constraint () TypeIndex Kind IndexedType
constraint2 = Equality () [TVariable (TypeIndex KType 1), TVariable (TypeIndex KType 2)]

constraint3 :: Constraint () TypeIndex Kind IndexedType
constraint3 = Implicit () (TVariable (TypeIndex KType 3)) (TVariable (TypeIndex KType 1)) (Monomorphic (Set.fromList [TypeIndex KType 0]))

constraint4 :: Constraint () TypeIndex Kind IndexedType
constraint4 = Equality () [TVariable (TypeIndex KType 2), TVariable (TypeIndex KType 0)]

collectELambdaConstraintsSpec4 :: Bool
collectELambdaConstraintsSpec4 =
  constraint2 `elem` constraints && constraint3 `elem` constraints && constraint4 `elem` constraints
 where
  constraints = muteConstraint <$> rights outs
  (ms, outs) = evalConstraintsGenStack (freshIdIn expr) ctx (collectConstraints expr)
  expr = fixture3
  ctx =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextDataConstructorEnv = mempty
      , constraintsGenContextCodataAccessorEnv = mempty
      , constraintsGenContextTypeConstructorEnv = mempty
      }
