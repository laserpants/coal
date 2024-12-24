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
import Noll.Language.Type (Type)
import qualified Noll.Language.Type as Type
import Noll.Language.Type.Index (TypeIndex (..))
import qualified Noll.Language.Type.Intrinsic as Intrinsic
import Noll.TypeSystem.Constraint (MonomorphicSet (..), TypeConstraint (..))
import Noll.TypeSystem.Constraint.Collect
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.Constraint.Collect" $ do
    it "" $
      hasConstraint
        fixture_1
        (Equality (typeVariable 2) (typeBool `Type.Arrow` typeVariable 3))
    it "" $
      hasConstraint
        fixture_1
        (Equality (typeVariable 5) (typeVariable 1))
    it "" $
      hasConstraint
        fixture_1
        (Equality (typeVariable 6) (typeVariable 1))
    it "" $
      hasConstraint
        fixture_1
        (Equality (typeVariable 7) (typeVariable 3))
    it "" $
      hasConstraint
        fixture_1
        (Implicit (typeVariable 4) (typeVariable 7) (MonomorphicSet (Set.fromList [TypeIndex () 5])))
    it "" $
      hasConstraint
        fixture_1
        (Implicit (typeVariable 2) (typeVariable 6) (MonomorphicSet (Set.fromList [TypeIndex () 5])))

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

typeBool :: Type TypeIndex ()
typeBool = Type.Intrinsic Intrinsic.Bool

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
    undefined
    (
      Expr.Application
        undefined
        undefined
        undefined
    )
