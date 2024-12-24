{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.SubstitutionSpec where

import Data.List.NonEmpty (NonEmpty (..))
import Noll.Label (Label (..))
import Noll.Language.Expression (Expression)
import qualified Noll.Language.Expression as Expr
import qualified Noll.Language.Expression.Binding as Binding
import Noll.Language.Kind.Index (KindIndex (..))
import qualified Noll.Language.Pattern as Pattern
import qualified Noll.Language.Primitive as Prim
import qualified Noll.Language.Primitive as Primitive
import Noll.Language.Type (Type)
import qualified Noll.Language.Type as Type
import Noll.Language.Type.Index (TypeIndex (..))
import qualified Noll.Language.Type.Intrinsic as Intrinsic
import Noll.Language.Type.Kind (Kind)
import qualified Noll.Language.Type.Kind as Kind
import Noll.TypeSystem.TypeSubstitution
import Noll.TypeSystem.KindSubstitution (mapsToKind, kindApply)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.Substitution" $ do
    describe "TypeSubstitution" $ do
      it "" $
        apply (1 `mapsTo` Type.Intrinsic Intrinsic.Bool) t1 == Type.Intrinsic Intrinsic.Bool
      it "" $
        apply (1 `mapsTo` Type.Intrinsic Intrinsic.Bool) t2 == t2
      it "" $
        applySubstitutionEquals
          fixture_1
          ( Expr.Lambda
              (Pattern.Variable (Label (typeBool `Type.Arrow` typeVariable 3) "m") :| [])
              ( Expr.Let
                  ( Binding.Pattern
                      (Pattern.Variable (Label (typeBool `Type.Arrow` typeVariable 3) "y"))
                      (Expr.Variable (Label (typeBool `Type.Arrow` typeVariable 3) "m"))
                      :| []
                  )
                  ( Expr.Let
                      ( Binding.Pattern
                          (Pattern.Variable (Label (typeVariable 3) "x"))
                          ( Expr.Application
                              (typeVariable 3)
                              (Expr.Variable (Label (typeBool `Type.Arrow` typeVariable 3) "y"))
                              (Expr.Literal (Prim.Bool True) :| [])
                          )
                          :| []
                      )
                      ( Expr.Variable (Label (typeVariable 3) "x")
                      )
                  )
              )
          )
      it "" $
        applySubstitutionEquals
          fixture_2
          ( Expr.Let
              ( Binding.Pattern
                  (Pattern.Variable (Label (typeVariable 3 `Type.Arrow` typeVariable 3) "f"))
                  ( Expr.Lambda
                      (Pattern.Variable (Label (typeVariable 3) "x") :| [])
                      (Expr.Variable (Label (typeVariable 3) "x"))
                  )
                  :| []
              )
              ( Expr.Application
                  typeInt32
                  ( Expr.Application
                      (typeInt32 `Type.Arrow` typeInt32)
                      (Expr.Variable (Label ((typeInt32 `Type.Arrow` typeInt32) `Type.Arrow` typeInt32 `Type.Arrow` typeInt32) "f"))
                      (Expr.Variable (Label (typeInt32 `Type.Arrow` typeInt32) "f") :| [])
                  )
                  ( Expr.Application
                      typeInt32
                      (Expr.Variable (Label (typeInt32 `Type.Arrow` typeInt32) "f"))
                      (Expr.Literal (Primitive.Int32 1) :| [])
                      :| []
                  )
              )
          )
    describe "KindSubstitution" $ do
      it "" $
        kindApply (1 `mapsToKind` Kind.Type) t3 == Type.Variable (TypeIndex Kind.Type 1)
      it "" $
        kindApply (2 `mapsToKind` Kind.Type) t3 == t3

applySubstitutionEquals :: (Expression (Type TypeIndex (Kind KindIndex)), TypeSubstitution) -> Expression (Type TypeIndex (Kind KindIndex)) -> Bool
applySubstitutionEquals (e, sub) res = apply sub e == res

t1 :: Type TypeIndex (Kind KindIndex)
t1 = Type.Variable (TypeIndex Kind.Type 1)

t2 :: Type TypeIndex (Kind KindIndex)
t2 = Type.Variable (TypeIndex Kind.Type 0)

t3 :: Type TypeIndex (Kind KindIndex)
t3 = Type.Variable (TypeIndex (Kind.Variable (KindIndex 1)) 1)

fixture_1 :: (Expression (Type TypeIndex (Kind KindIndex)), TypeSubstitution)
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
  , typeSubstitutionFromList
      [ (1, typeBool `Type.Arrow` typeVariable 3)
      , (2, typeBool `Type.Arrow` typeVariable 3)
      , (4, typeVariable 3)
      , (5, typeBool `Type.Arrow` typeVariable 3)
      , (6, typeBool `Type.Arrow` typeVariable 3)
      , (7, typeVariable 3)
      ]
  )

fixture_2 :: (Expression (Type TypeIndex (Kind KindIndex)), TypeSubstitution)
fixture_2 =
  ( Expr.Let
      ( Binding.Pattern
          (Pattern.Variable (Label (typeVariable 1) "f"))
          ( Expr.Lambda
              (Pattern.Variable (Label (typeVariable 2) "x") :| [])
              (Expr.Variable (Label (typeVariable 3) "x"))
          )
          :| []
      )
      ( Expr.Application
          (typeVariable 4)
          ( Expr.Application
              (typeVariable 5)
              (Expr.Variable (Label (typeVariable 6) "f"))
              (Expr.Variable (Label (typeVariable 7) "f") :| [])
          )
          ( Expr.Application
              (typeVariable 8)
              (Expr.Variable (Label (typeVariable 9) "f"))
              (Expr.Literal (Primitive.Int32 1) :| [])
              :| []
          )
      )
  , typeSubstitutionFromList
      [ (1, typeVariable 3 `Type.Arrow` typeVariable 3)
      , (2, typeVariable 3)
      , (4, typeInt32)
      , (5, typeInt32 `Type.Arrow` typeInt32)
      , (6, (typeInt32 `Type.Arrow` typeInt32) `Type.Arrow` typeInt32 `Type.Arrow` typeInt32)
      , (7, typeInt32 `Type.Arrow` typeInt32)
      , (8, typeInt32)
      , (9, typeInt32 `Type.Arrow` typeInt32)
      ]
  )

typeVariable :: Int -> Type TypeIndex (Kind KindIndex)
typeVariable = Type.Variable . TypeIndex Kind.Type

typeBool :: Type TypeIndex k
typeBool = Type.Intrinsic Intrinsic.Bool

typeInt32 :: Type TypeIndex k
typeInt32 = Type.Intrinsic Intrinsic.Int32
