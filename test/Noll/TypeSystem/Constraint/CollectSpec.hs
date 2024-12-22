{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.Constraint.CollectSpec where

import Data.List.NonEmpty (NonEmpty (..))
import Noll.Label (Label (..))
import Noll.Language.Expression (Expression)
import qualified Noll.Language.Expression as Expr
import qualified Noll.Language.Expression.Binding as Binding
import qualified Noll.Language.Pattern as Pattern
import qualified Noll.Language.Primitive as Prim
import qualified Noll.Language.Type as Type
import Noll.Language.Type.Index (TypeIndex (..))
import Noll.Language.Type.Opaque (OpaqueType)
import Noll.TypeSystem.Constraint.Collect
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.Constraint.Collect" $ do
    it "" $ do
      1 == 1

collectFixture_1 :: Expression Int
collectFixture_1 =
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

collectFixture_2 :: Expression OpaqueType
collectFixture_2 = fmap (Type.Variable . TypeIndex ()) collectFixture_1

-- collectFixture_3 :: (([Assumption OpaqueType], Expression OpaqueType), [TypeConstraint TypeIndex () OpaqueType])
collectFixture_3 =
  runCollectConstraints
    (ConstraintsContext mempty)
    (collectConstraints collectFixture_2)
