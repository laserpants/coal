{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.Constraint.AggregationSpec where

import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Expression (..),
  Kind (..),
  Pattern (..),
  Primitive (..),
  Scheme (..),
  Type (..),
  TypeIndex (..),
  TypeParam (..),
  freshIdIn,
 )
import Noll.TypeSystem.Constraint.Aggregation
import Noll.TypeSystem.Constraint.Aggregation.Internal
import Noll.TypeSystem.Constraint.Aggregation.TypeAnnotation
import Noll.TypeSystem.Constraint.Assumption (Assumption (..))
import Test.Hspec (Spec, describe, it)

import qualified Data.Set as Set

spec :: Spec
spec =
  describe "Noll.TypeSystem.Constraint.Aggregation" $ do
    --    describe "collectConstraints" $ do
    --      describe "ELet" $ do
    --        it "" $
    --          1 == 0
    describe "instantiateAnnotation" $ do
      describe "Valid" $ do
        it "a -> b" $
          testRunner2
            fixture13
            == Right (TArrow (TVariable (TypeIndex KType 0)) (TVariable (TypeIndex KType 1)))
        it "a -> a" $
          testRunner2
            fixture14
            == Right (TArrow (TVariable (TypeIndex KType 0)) (TVariable (TypeIndex KType 0)))
        it "f(a) -> f(b)" $
          testRunner2
            fixture10
            == Right
              ( TArrow
                  (TApplication KType (TVariable (TypeIndex (KArrow KType KType) 5)) (TVariable (TypeIndex KType 0) :| []))
                  (TApplication KType (TVariable (TypeIndex (KArrow KType KType) 5)) (TVariable (TypeIndex KType 1) :| []))
              )
        it "f(a) -> f(a)" $
          testRunner2
            fixture11
            == Right
              ( TArrow
                  (TApplication KType (TVariable (TypeIndex (KArrow KType KType) 5)) (TVariable (TypeIndex KType 0) :| []))
                  (TApplication KType (TVariable (TypeIndex (KArrow KType KType) 5)) (TVariable (TypeIndex KType 0) :| []))
              )
      describe "Kind mismatch" $ do
        it "f -> f(a)" $
          testRunner2 fixture12 == Left (KindMismatch ())

-- typeConstraintsInclude :: forall a. (Show a, Eq a) => Expression a Int -> TypeConstraint (TypeRule () a) TypeIndex () (Type TypeIndex ()) -> Bool
typeConstraintsInclude e r =
  undefined

--  let
--    e1 = fmap typeVariable e
--
--    res0 :: ([TypeCollectError a], [TypeConstraint (TypeRule () a) TypeIndex () (Type TypeIndex ())])
--    res0 =
--      evalCollectTypeConstraints
--        (TypeConstraintsContext mempty constructorEnv)
--        (collectTypeConstraints e1)
--
--    (_, constraints) = res0
--   in
--    --    traceShow constraints $
--    case sample of
--      Equality meta ts ->
--        elem (normalized (Equality meta ts)) (normalized <$> constraints)
--      c ->
--        elem c constraints
-- where
--  normalized =
--    \case
--      Equality meta ts ->
--        Equality meta (sort ts)
--      c ->
--        c

testRunner ::
  Expression a Int ->
  ( [Assumption (Type TypeIndex Kind)]
  , [AggregationOutput a TypeIndex Kind (Type TypeIndex Kind)]
  )
testRunner e =
  let
    e0 = toIndexed e
   in
    evalAggregationStack
      (ConstraintsGenerationContext mempty mempty mempty (freshIdIn e0))
      (collectConstraints e0)

testRunner2 :: Type TypeParam () -> Either (TypeAnnotationError ()) (Type TypeIndex Kind)
testRunner2 t = s
 where
  (s, _) =
    evalAggregationStack
      (ConstraintsGenerationContext mempty mempty mempty 0)
      (instantiateAnnotation () t)

toIndexed :: Expression a Int -> Expression a (Type TypeIndex Kind)
toIndexed = fmap (TVariable . TypeIndex KType)

-- fn(m) => let y = m in let x = y(true) in x
fixture1 :: Expression () Int
fixture1 =
  ELambda
    ()
    (PVariable () (Label 5 "m") :| [])
    ( ELet
        ()
        ( BPattern
            ()
            (PVariable () (Label 6 "y"))
            (EVariable () (Label 1 "m"))
            :| []
        )
        ( ELet
            ()
            ( BPattern
                ()
                (PVariable () (Label 7 "x"))
                ( EApplication
                    ()
                    3
                    (EVariable () (Label 2 "y"))
                    (ELiteral () (LBool True) :| [])
                )
                :| []
            )
            ( EVariable () (Label 4 "x")
            )
        )
    )

-- let x = 5 in x
fixture7 :: Expression String Int
fixture7 =
  ELet
    "ELet"
    ( BPattern
        "BPattern"
        (PVariable "PVariable" (Label 1 "x"))
        (ELiteral "ELiteral" (LInt32 5))
        :| []
    )
    (EVariable "EVariable" (Label 2 "x"))

-- let x = 5 in y
fixture8 :: Expression String Int
fixture8 =
  ELet
    "ELet"
    ( BPattern
        "BPattern"
        (PVariable "PVariable" (Label 1 "x"))
        (ELiteral "ELiteral" (LInt32 5))
        :| []
    )
    (EVariable "EVariable" (Label 2 "y"))

-- let x = 5 in 1
fixture9 :: Expression String Int
fixture9 =
  ELet
    "ELet"
    ( BPattern
        "BPattern"
        (PVariable "PVariable" (Label 1 "x"))
        (ELiteral "ELiteral" (LInt32 5))
        :| []
    )
    (ELiteral "ELiteral" (LInt32 1))

fixture10 :: Type TypeParam ()
fixture10 =
  TArrow
    (TApplication () (TVariable (TypeParam () "f")) (TVariable (TypeParam () "a") :| []))
    (TApplication () (TVariable (TypeParam () "f")) (TVariable (TypeParam () "b") :| []))

fixture11 :: Type TypeParam ()
fixture11 =
  TArrow
    (TApplication () (TVariable (TypeParam () "f")) (TVariable (TypeParam () "a") :| []))
    (TApplication () (TVariable (TypeParam () "f")) (TVariable (TypeParam () "a") :| []))

fixture12 :: Type TypeParam ()
fixture12 =
  TArrow
    (TVariable (TypeParam () "f"))
    (TApplication () (TVariable (TypeParam () "f")) (TVariable (TypeParam () "a") :| []))

-- a -> b
fixture13 :: Type TypeParam ()
fixture13 = TArrow (TVariable (TypeParam () "a")) (TVariable (TypeParam () "b"))

-- a -> a
fixture14 :: Type TypeParam ()
fixture14 = TArrow (TVariable (TypeParam () "a")) (TVariable (TypeParam () "a"))
