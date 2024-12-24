{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.SubstitutionSpec where

import Data.List.NonEmpty (NonEmpty (..))
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
import Noll.Language.Type.Kind (Kind)
import qualified Noll.Language.Type.Kind as Kind
import Noll.TypeSystem.Substitution
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.Substitution" $ do
    it "" $
      apply (1 `mapsTo` Type.Intrinsic Intrinsic.Bool) t1 == Type.Intrinsic Intrinsic.Bool
    it "" $
      apply (1 `mapsTo` Type.Intrinsic Intrinsic.Bool) t2 == t2
    it "" $
      x123 fixture_1 $
        Expr.Variable (Label (typeVariable 0) "f")

x123 (e, sub) res =
  apply sub e == res

t1 :: Type TypeIndex (Kind Int)
t1 = Type.Variable (TypeIndex Kind.Type 1)

t2 :: Type TypeIndex (Kind Int)
t2 = Type.Variable (TypeIndex Kind.Type 0)

fixture_1 :: (Expression (Type TypeIndex (Kind Int)), TypeSubstitution)
fixture_1 =
  ( Expr.Lambda
      (Pattern.Variable (Label (typeVariable 5) "m") :| [])
      ( Expr.Let
          ( Binding.Pattern
              (Pattern.Variable (Label (typeVariable 6) "y"))
              (Expr.Variable (Label (typeVariable 1) "m"))
              :| []
          )
          ( Expr.Let
              ( Binding.Pattern
                  (Pattern.Variable (Label (typeVariable 7) "x"))
                  ( Expr.Application
                      (typeVariable 3)
                      (Expr.Variable (Label (typeVariable 2) "y"))
                      (Expr.Literal (Prim.Bool True) :| [])
                  )
                  :| []
              )
              ( Expr.Variable (Label (typeVariable 4) "x")
              )
          )
      )
  , substitutionFromList
      [ (1, typeBool `Type.Arrow` typeVariable 3)
      , (2, typeBool `Type.Arrow` typeVariable 3)
      , (4, typeVariable 3)
      , (5, typeBool `Type.Arrow` typeVariable 3)
      ]
  )

typeVariable :: Int -> Type TypeIndex (Kind Int)
typeVariable = Type.Variable . TypeIndex Kind.Type

typeBool :: Type TypeIndex k
typeBool = Type.Intrinsic Intrinsic.Bool

typeInt32 :: Type TypeIndex k
typeInt32 = Type.Intrinsic Intrinsic.Int32
