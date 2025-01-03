{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemSpec where

import Control.Monad.State (evalState)
import Data.Either.Extra (lefts, rights)
import Noll.Label (Label (..))
import Noll.Language (Binding (..), Expression (..), Intrinsic (..), Kind (..), Pattern (..), Primitive (..), Type (..), TypeIndex (..))
import Noll.Library.List1 (NonEmpty (..))
import Noll.Library.Supply (supply)
import Noll.TypeSystem.Constraint.Aggregation
import Noll.TypeSystem.Constraint.Solver (solveConstraints)
import Noll.TypeSystem.Substitution (apply, normalizeTypeIndexes)
import Test.Hspec (Spec, describe, hspec, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem" $ do
    it "" $
      testRunner fixture1 == fixture1Typed

testRunner :: Expression () () -> Expression () (Type TypeIndex Kind)
testRunner e =
  let
    e0 = evalState (traverse (const supply) e) (0 :: Int)

    e1 = toIndexed e0

    (_, out) =
      runAggregationStack
        (AggregationContext mempty mempty mempty)
        (aggregateConstraints e1)

    errors = lefts out
    constraints = rights out

    res0 = solveConstraints constraints

    (sub, _) = res0

    e2 = apply sub e1

    e3 = normalizeTypeIndexes e2
   in
    e3

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
