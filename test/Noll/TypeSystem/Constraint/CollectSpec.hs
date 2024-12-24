{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.Constraint.CollectSpec where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Noll.Label (Label (..))
import Noll.Language.Expression (Expression)
import qualified Noll.Language.Expression as Expr
import qualified Noll.Language.Expression.Binding as Binding
import qualified Noll.Language.Pattern as Pattern
import qualified Noll.Language.Primitive as Prim
import qualified Noll.Language.Primitive as Primitive
import Noll.Language.Type (Type)
import qualified Noll.Language.Type as Type
import Noll.Language.Type.Index (TypeIndex (..))
import qualified Noll.Language.Type.Intrinsic as Intrinsic
import Noll.TypeSystem.TypeConstraint (MonomorphicSet (..), TypeConstraint (..))
import Noll.TypeSystem.Constraint.Collect
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.Constraint.Collect" $ do
    describe "fixture_1" $ do
      it "" $
        hasConstraints
          fixture_1
          [ (Equality (typeVariable 2) (typeBool `Type.Arrow` typeVariable 3))
          , (Equality (typeVariable 5) (typeVariable 1))
          , (Equality (typeVariable 6) (typeVariable 1))
          , (Equality (typeVariable 7) (typeVariable 3))
          , (Implicit (typeVariable 4) (typeVariable 7) (MonomorphicSet (Set.fromList [TypeIndex () 5])))
          , (Implicit (typeVariable 2) (typeVariable 6) (MonomorphicSet (Set.fromList [TypeIndex () 5])))
          ]
    describe "fixture_2" $ do
      it "" $
        hasConstraints
          fixture_2
          [ (Implicit (typeVariable 6) (typeVariable 1) (MonomorphicSet mempty))
          , (Implicit (typeVariable 7) (typeVariable 1) (MonomorphicSet mempty))
          , (Implicit (typeVariable 9) (typeVariable 1) (MonomorphicSet mempty))
          , (Equality (typeVariable 2) (typeVariable 3))
          , (Equality (typeVariable 6) (typeVariable 7 `Type.Arrow` typeVariable 5))
          , (Equality (typeVariable 9) (typeInt32 `Type.Arrow` typeVariable 8))
          , (Equality (typeVariable 1) (typeVariable 2 `Type.Arrow` typeVariable 3))
          , (Equality (typeVariable 5) (typeVariable 8 `Type.Arrow` typeVariable 4))
          ]

hasConstraints :: Expression Int -> [TypeConstraint TypeIndex () (Type TypeIndex ())] -> Bool
hasConstraints = all . hasConstraint

hasConstraint :: Expression Int -> TypeConstraint TypeIndex () (Type TypeIndex ()) -> Bool
hasConstraint e =
  \case
    Equality t1 t2 ->
      elem (Equality t1 t2) constraints || elem (Equality t2 t1) constraints
    c ->
      elem c constraints
 where
  constraints =
    evalCollectConstraints
      (ConstraintsContext mempty)
      (collectConstraints (fmap typeVariable e))

typeVariable :: Int -> Type TypeIndex ()
typeVariable = Type.Variable . TypeIndex ()

typeBool :: Type TypeIndex k
typeBool = Type.Intrinsic Intrinsic.Bool

typeInt32 :: Type TypeIndex k
typeInt32 = Type.Intrinsic Intrinsic.Int32

-- fn(m) => let y = m in let x = y(true) in x
fixture_1 :: Expression Int
fixture_1 =
  Expr.Lambda
    (Pattern.Variable (Label 5 "m") :| [])
    ( Expr.Let
        ( Binding.Pattern
            (Pattern.Variable (Label 6 "y"))
            (Expr.Variable (Label 1 "m"))
            :| []
        )
        ( Expr.Let
            ( Binding.Pattern
                (Pattern.Variable (Label 7 "x"))
                ( Expr.Application
                    3
                    (Expr.Variable (Label 2 "y"))
                    (Expr.Literal (Prim.Bool True) :| [])
                )
                :| []
            )
            ( Expr.Variable (Label 4 "x")
            )
        )
    )

-- let f = fn(x) => x in (f f)(f 1)
fixture_2 :: Expression Int
fixture_2 =
  Expr.Let
    ( Binding.Pattern
        (Pattern.Variable (Label 1 "f"))
        ( Expr.Lambda
            (Pattern.Variable (Label 2 "x") :| [])
            (Expr.Variable (Label 3 "x"))
        )
        :| []
    )
    ( Expr.Application
        4
        ( Expr.Application
            5
            (Expr.Variable (Label 6 "f"))
            (Expr.Variable (Label 7 "f") :| [])
        )
        ( Expr.Application
            8
            (Expr.Variable (Label 9 "f"))
            (Expr.Literal (Primitive.Int32 1) :| [])
            :| []
        )
    )
