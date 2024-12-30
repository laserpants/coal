{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.TypeConstraint.CollectSpec (spec) where

import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.Set as Set
import Debug.Trace
import Noll.Label (Label (..))
import Noll.Language (Binding (..), Choice (..), Clause (..), Constructor (..), Expression (..), Intrinsic (..), Kind (..), KindIndex (..), Pattern (..), Primitive (..), Scheme (..), Type (..), TypeIndex (..), freshIdIn)
import Noll.Library.Environment (Environment)
import qualified Noll.Library.Environment as Environment
import Noll.TypeSystem.TypeConstraint (MonomorphicSet (..), TypeConstraint (..), TypeConstraintMetadata (..))
import Noll.TypeSystem.TypeConstraint.Collect (TypeConstraintsContext (..), collectTypeConstraints, evalCollectTypeConstraints)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.TypeConstraint.Collect" $ do
    describe "collectTypeConstraints" $ do
      it "fn(m) => let y = m in let x = y(true) in x" $
        typeConstraintsIncludeAll
          fixture1
          [ ( Equality
                (ConstraintApplication () (typeVariable 2) (typeBool `TArrow` typeVariable 3))
                [typeVariable 2, typeBool `TArrow` typeVariable 3]
            )
          , (Equality TypeConstraintMetadata [typeVariable 5, typeVariable 1])
          , (Equality TypeConstraintMetadata [typeVariable 6, typeVariable 1])
          , (Equality TypeConstraintMetadata [typeVariable 7, typeVariable 3])
          , (Implicit TypeConstraintMetadata (typeVariable 4) (typeVariable 7) (MonomorphicSet (Set.fromList [TypeIndex () 5])))
          , (Implicit TypeConstraintMetadata (typeVariable 2) (typeVariable 6) (MonomorphicSet (Set.fromList [TypeIndex () 5])))
          ]
      it "let f = fn(x) => x in (f(f))(f(1))" $ do
        typeConstraintsIncludeAll
          fixture2
          [ (Implicit TypeConstraintMetadata (typeVariable 6) (typeVariable 1) (MonomorphicSet mempty))
          , (Implicit TypeConstraintMetadata (typeVariable 7) (typeVariable 1) (MonomorphicSet mempty))
          , (Implicit TypeConstraintMetadata (typeVariable 9) (typeVariable 1) (MonomorphicSet mempty))
          , (Equality TypeConstraintMetadata [typeVariable 2, typeVariable 3])
          , ( Equality
                (ConstraintApplication () (typeVariable 6) (typeVariable 7 `TArrow` typeVariable 5))
                [typeVariable 6, typeVariable 7 `TArrow` typeVariable 5]
            )
          , ( Equality
                (ConstraintApplication () (typeVariable 9) (typeInt32 `TArrow` typeVariable 8))
                [typeVariable 9, typeInt32 `TArrow` typeVariable 8]
            )
          , (Equality TypeConstraintMetadata [typeVariable 1, typeVariable 2 `TArrow` typeVariable 3])
          , ( Equality
                (ConstraintApplication () (typeVariable 5) (typeVariable 8 `TArrow` typeVariable 4))
                [typeVariable 5, typeVariable 8 `TArrow` typeVariable 4]
            )
          ]
      it "let x = 1 in x(x)" $ do
        typeConstraintsIncludeAll
          fixture3
          [ ( Equality
                (ConstraintApplication () (typeVariable 2) (typeVariable 3 `TArrow` typeVariable 1))
                [typeVariable 2, typeVariable 3 `TArrow` typeVariable 1]
            )
          , (Equality TypeConstraintMetadata [typeVariable 0, typeInt32])
          , (Implicit TypeConstraintMetadata (typeVariable 2) (typeVariable 0) (MonomorphicSet mempty))
          , (Implicit TypeConstraintMetadata (typeVariable 3) (typeVariable 0) (MonomorphicSet mempty))
          ]
      it "match x { | Yes => true }" $ do
        typeConstraintsIncludeAll
          fixture4
          [ Equality (ConstraintMatchClauseExpressions ()) [typeVariable 0, TIntrinsic IBool]
          , Equality (ConstraintMatchClausePatterns ()) [typeVariable 1, typeVariable 2]
          , Explicit TypeConstraintMetadata (typeVariable 2) (Forall mempty [] (TConstructor () "Answer"))
          ]

typeConstraintsIncludeAll :: (Show a, Eq a) => Expression a Int -> [TypeConstraint (TypeConstraintMetadata () a) TypeIndex () (Type TypeIndex ())] -> Bool
typeConstraintsIncludeAll = all . typeConstraintsInclude

typeConstraintsInclude :: (Show a, Eq a) => Expression a Int -> TypeConstraint (TypeConstraintMetadata () a) TypeIndex () (Type TypeIndex ()) -> Bool
typeConstraintsInclude e =
  --  traceShow constraints $
  \case
    Equality meta ts ->
      elem (normalized (Equality meta ts)) (normalized <$> constraints)
    c ->
      elem c constraints
 where
  constraints =
    let e' = fmap typeVariable e
     in evalCollectTypeConstraints
          (freshIdIn e')
          (TypeConstraintsContext mempty constructorEnv)
          (collectTypeConstraints e')
  normalized =
    \case
      Equality meta ts ->
        Equality meta (sort ts)
      c ->
        c

constructorEnv :: Environment (Constructor TypeIndex () (Type TypeIndex ()))
constructorEnv =
  Environment.fromList
    [
      ( "Yes"
      , Constructor "Yes" 0 (Forall mempty [] (TConstructor () "Answer"))
      )
    ,
      ( "No"
      , Constructor "No" 0 (Forall mempty [] (TConstructor () "Answer"))
      )
    ,
      ( "Foo"
      , Constructor "Foo" 0 (Forall mempty [] (TConstructor () "Foo"))
      )
    ,
      ( "Id"
      , Constructor
          "Id"
          1
          ( Forall
              mempty
              []
              ( TVariable (TypeIndex () 0)
                  `TArrow` TApplication
                    ()
                    (TConstructor () "Id")
                    (TVariable (TypeIndex () 0) :| [])
              )
          )
      )
    ]

typeVariable :: Int -> Type TypeIndex ()
typeVariable = TVariable . TypeIndex ()

typeBool :: Type TypeIndex k
typeBool = TIntrinsic IBool

typeInt32 :: Type TypeIndex k
typeInt32 = TIntrinsic IInt32

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

-- let f = fn(x) => x in (f(f))(f(1))
fixture2 :: Expression () Int
fixture2 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label 1 "f"))
        ( ELambda
            ()
            (PVariable () (Label 2 "x") :| [])
            (EVariable () (Label 3 "x"))
        )
        :| []
    )
    ( EApplication
        ()
        4
        ( EApplication
            ()
            5
            (EVariable () (Label 6 "f"))
            (EVariable () (Label 7 "f") :| [])
        )
        ( EApplication
            ()
            8
            (EVariable () (Label 9 "f"))
            (ELiteral () (LInt32 1) :| [])
            :| []
        )
    )

-- let x = 1 in x(x)
fixture3 :: Expression () Int
fixture3 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label 0 "x"))
        (ELiteral () (LInt32 1))
        :| []
    )
    ( EApplication
        ()
        1
        (EVariable () (Label 2 "x"))
        (EVariable () (Label 3 "x") :| [])
    )

-- match x { | Yes => true }
fixture4 :: Expression () Int
fixture4 =
  ( EMatch
      ()
      0
      (EVariable () (Label 1 "x"))
      ( EClause
          ()
          (PConstructor () (Label 2 "Yes") [])
          (CPlain () [] (ELiteral () (LBool True)) :| [])
          :| []
      )
  )
