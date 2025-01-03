{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemSpec where

import Control.Monad.State (evalState)
import Data.Either.Extra (lefts, rights)
import Noll.Label (Label (..))
import Noll.Language (Binding (..), Expression (..), IndexedType, Intrinsic (..), Kind (..), Pattern (..), Primitive (..), Type (..), TypeIndex (..))
import Noll.Library.List1 (NonEmpty (..))
import Noll.Library.Supply (supply)
import Noll.TypeSystem.Constraint.Aggregation
import Noll.TypeSystem.Constraint.Rule (Assumption (..), InferenceRule (..))
import Noll.TypeSystem.Constraint.Solver (solveConstraints)
import Noll.TypeSystem.Substitution (apply, normalizeTypeIndexes)
import Test.Hspec (Spec, describe, hspec, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem" $ do
    it "fn(m) => let y = m in let x = y(true) in x" $
      validateResult fixture1 == fixture1Typed
    it "let f = fn(x) => x in (f(f))(f(1))" $
      validateResult fixture2 == fixture2Typed

-- validateSolverErrors :: Expression () () -> Expression () (Type TypeIndex Kind)
-- validateSolverErrors ::

validateResult :: Expression () () -> Expression () (Type TypeIndex Kind)
validateResult e = e1
 where
  (e1, _, _, _) = testRunner e

testRunner ::
  Expression () () ->
  ( Expression () (Type TypeIndex Kind)
  , [Assumption IndexedType]
  , [AggregationError ()]
  , [InferenceRule Kind ()]
  )
testRunner e =
  let
    e0 = evalState (traverse (const supply) e) (0 :: Int)

    e1 = toIndexed e0

    (asms, out) =
      runAggregationStack
        (AggregationContext mempty mempty mempty)
        (aggregateConstraints e1)

    errors0 = lefts out
    constraints = rights out

    res0 = solveConstraints constraints

    (sub, errors1) = res0

    e2 = apply sub e1

    e3 = normalizeTypeIndexes e2
   in
    (e3, asms, errors0, apply sub errors1)

toIndexed :: Expression a Int -> Expression a (Type TypeIndex Kind)
toIndexed = fmap (TVariable . TypeIndex KType)

-- fn(m) => let y = m in let x = y(true) in x
fixture1 :: Expression () ()
fixture1 =
  ELambda
    ()
    (PVariable () (Label () "m") :| [])
    ( ELet
        ()
        ( BPattern
            ()
            (PVariable () (Label () "y"))
            (EVariable () (Label () "m"))
            :| []
        )
        ( ELet
            ()
            ( BPattern
                ()
                (PVariable () (Label () "x"))
                ( EApplication
                    ()
                    ()
                    (EVariable () (Label () "y"))
                    (ELiteral () (LBool True) :| [])
                )
                :| []
            )
            ( EVariable () (Label () "x")
            )
        )
    )

fixture1Typed :: Expression () (Type TypeIndex Kind)
fixture1Typed =
  ( ELambda
      ()
      (PVariable () (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex KType 0)) "m") :| [])
      ( ELet
          ()
          ( BPattern
              ()
              (PVariable () (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex KType 0)) "y"))
              (EVariable () (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex KType 0)) "m"))
              :| []
          )
          ( ELet
              ()
              ( BPattern
                  ()
                  (PVariable () (Label (TVariable (TypeIndex KType 0)) "x"))
                  ( EApplication
                      ()
                      (TVariable (TypeIndex KType 0))
                      (EVariable () (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex KType 0)) "y"))
                      (ELiteral () (LBool True) :| [])
                  )
                  :| []
              )
              ( EVariable () (Label (TVariable (TypeIndex KType 0)) "x")
              )
          )
      )
  )

-- let f = fn(x) => x in (f(f))(f(1))
fixture2 :: Expression () ()
fixture2 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label () "f"))
        ( ELambda
            ()
            (PVariable () (Label () "x") :| [])
            (EVariable () (Label () "x"))
        )
        :| []
    )
    ( EApplication
        ()
        ()
        ( EApplication
            ()
            ()
            (EVariable () (Label () "f"))
            (EVariable () (Label () "f") :| [])
        )
        ( EApplication
            ()
            ()
            (EVariable () (Label () "f"))
            (ELiteral () (LInt32 1) :| [])
            :| []
        )
    )

fixture2Typed :: Expression () (Type TypeIndex Kind)
fixture2Typed =
  ( ELet
      ()
      ( BPattern
          ()
          (PVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0)) "f"))
          ( ELambda
              ()
              (PVariable () (Label (TVariable (TypeIndex KType 0)) "x") :| [])
              (EVariable () (Label (TVariable (TypeIndex KType 0)) "x"))
          )
          :| []
      )
      ( EApplication
          ()
          (TIntrinsic IInt32)
          ( EApplication
              ()
              (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
              (EVariable () (Label ((TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f"))
              (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f") :| [])
          )
          ( EApplication
              ()
              (TIntrinsic IInt32)
              (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f"))
              (ELiteral () (LInt32 1) :| [])
              :| []
          )
      )
  )

-- TODO
-- let x = 1 in x(x)
fixture3 :: Expression () ()
fixture3 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label () "x"))
        (ELiteral () (LInt32 1))
        :| []
    )
    ( EApplication
        ()
        ()
        (EVariable () (Label () "x"))
        (EVariable () (Label () "x") :| [])
    )
