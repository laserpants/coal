{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.SystemF.Constraint.GenerationSpec where

import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Debug.Trace
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
  indexed,
 )
import Noll.SystemF.Constraint.Assumption (Assumption (..))
import Noll.SystemF.Constraint.Generation
import Noll.SystemF.Constraint.Generation.Internal
import Noll.SystemF.Constraint.Generation.TypeAnnotation
import Test.Hspec (Spec, describe, it)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec =
  describe "Noll.SystemF.Constraint.Generation" $ do
    --    describe "collectConstraints" $ do
    --      describe "ELet" $ do
    --        it "" $
    --          1 == 0
    describe "instantiateAnnotation" $ do
      describe "Valid" $ do
        it "a -> b" $
          testInstantiateAnnotation
            fixture13
            == Right (TArrow (TVariable (TypeIndex KType (-1))) (TVariable (TypeIndex KType (-2))))
        it "a -> a" $
          testInstantiateAnnotation
            fixture14
            == Right (TArrow (TVariable (TypeIndex KType (-1))) (TVariable (TypeIndex KType (-1))))
        it "f(a) -> f(b)" $
          testInstantiateAnnotation
            fixture10
            == Right
              ( TArrow
                  (TApplication KType (TVariable (TypeIndex (KArrow KType KType) (-6))) (TVariable (TypeIndex KType (-1)) :| []))
                  (TApplication KType (TVariable (TypeIndex (KArrow KType KType) (-6))) (TVariable (TypeIndex KType (-2)) :| []))
              )
        it "f(a) -> f(a)" $
          testInstantiateAnnotation
            fixture11
            == Right
              ( TArrow
                  (TApplication KType (TVariable (TypeIndex (KArrow KType KType) (-6))) (TVariable (TypeIndex KType (-1)) :| []))
                  (TApplication KType (TVariable (TypeIndex (KArrow KType KType) (-6))) (TVariable (TypeIndex KType (-1)) :| []))
              )
      describe "Kind mismatch" $ do
        it "f -> f(a)" $
          testInstantiateAnnotation fixture12 == Left (KindMismatch ())

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

-- testCollectConstraints
testCollectConstraints ::
  (Show a) =>
  Expression a a ->
  ( [Assumption (Type TypeIndex Kind)]
  , [ConstraintsGenerationOutput a TypeIndex Kind (Type TypeIndex Kind)]
  )
testCollectConstraints e =
  let
    e0 = indexed e
   in
    traceShow e0 $
      evalConstraintsGenerationStack
        (freshIdIn e0)
        (ConstraintsGenerationContext mempty mempty mempty)
        (collectConstraints e0)

testInstantiateAnnotation :: Type TypeParam () -> Either (TypeAnnotationError ()) (Type TypeIndex Kind)
testInstantiateAnnotation t = s
 where
  (s, _) =
    evalConstraintsGenerationStack
      0
      (ConstraintsGenerationContext mempty mempty mempty)
      (instantiateAnnotation () t)

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

-- fn({ foo = 1 }) => foo
fixture15 :: Expression () ()
fixture15 =
  ELambda
    ()
    ( PRecord
        ()
        ()
        ( Map.fromList
            [
              ( "foo"
              , PLiteral () (LInt32 1)
              )
            ]
        )
        Nothing
        :| []
    )
    ( EVariable () (Label () "foo")
    )
