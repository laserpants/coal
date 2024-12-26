{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.TypeConstraint.CollectSpec where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Noll.Label (Label (..))
import Noll.Language (Binding (..), Expression (..), Intrinsic (..), Pattern (..), Primitive (..), Type (..), TypeIndex (..))
import Noll.TypeSystem.TypeConstraint (MonomorphicSet (..), TypeConstraint (..))
import Noll.TypeSystem.TypeConstraint.Collect
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.TypeConstraint.Collect" $ do
    describe "fixture1" $ do
      it "" $
        hasConstraints
          fixture1
          [ (Equality (typeVariable 2) (typeBool `TArrow` typeVariable 3))
          , (Equality (typeVariable 5) (typeVariable 1))
          , (Equality (typeVariable 6) (typeVariable 1))
          , (Equality (typeVariable 7) (typeVariable 3))
          , (Implicit (typeVariable 4) (typeVariable 7) (MonomorphicSet (Set.fromList [TypeIndex () 5])))
          , (Implicit (typeVariable 2) (typeVariable 6) (MonomorphicSet (Set.fromList [TypeIndex () 5])))
          ]
    describe "fixture2" $ do
      it "" $
        hasConstraints
          fixture2
          [ (Implicit (typeVariable 6) (typeVariable 1) (MonomorphicSet mempty))
          , (Implicit (typeVariable 7) (typeVariable 1) (MonomorphicSet mempty))
          , (Implicit (typeVariable 9) (typeVariable 1) (MonomorphicSet mempty))
          , (Equality (typeVariable 2) (typeVariable 3))
          , (Equality (typeVariable 6) (typeVariable 7 `TArrow` typeVariable 5))
          , (Equality (typeVariable 9) (typeInt32 `TArrow` typeVariable 8))
          , (Equality (typeVariable 1) (typeVariable 2 `TArrow` typeVariable 3))
          , (Equality (typeVariable 5) (typeVariable 8 `TArrow` typeVariable 4))
          ]

hasConstraints :: Expression a Int -> [TypeConstraint TypeIndex () (Type TypeIndex ())] -> Bool
hasConstraints = all . hasConstraint

hasConstraint :: Expression a Int -> TypeConstraint TypeIndex () (Type TypeIndex ()) -> Bool
hasConstraint e =
  \case
    Equality t1 t2 ->
      elem (Equality t1 t2) constraints || elem (Equality t2 t1) constraints
    c ->
      elem c constraints
 where
  constraints =
    evalCollectTypeConstraints
      (TypeConstraintsContext mempty mempty)
      (collectConstraints (fmap typeVariable e))

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
            (PVariable () (Label 6 "y"))
            (EVariable () (Label 1 "m"))
            :| []
        )
        ( ELet
            ()
            ( BPattern
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

-- let f = fn(x) => x in (f f)(f 1)
fixture2 :: Expression () Int
fixture2 =
  ELet
    ()
    ( BPattern
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
