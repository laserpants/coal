{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.KindSubstitutionSpec where

import Data.List.NonEmpty (NonEmpty (..))
import Noll.Label (Label (..))
import Noll.Language (Expression, Kind, KindIndex (..), Type, TypeIndex (..))
import qualified Noll.Language.Expression as Expr
import qualified Noll.Language.Expression.Binding as Binding
import qualified Noll.Language.Pattern as Pattern
import qualified Noll.Language.Primitive as Prim
import qualified Noll.Language.Type as Type
import qualified Noll.Language.Type.Intrinsic as Intrinsic
import qualified Noll.Language.Type.Kind as Kind
import Noll.TypeSystem.KindSubstitution (applyKindSub, mapsToKind)
import Noll.TypeSystem.TypeSubstitution (TypeSubstitution (..), apply, typeSubstitutionFromList)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.KindSubstitution" $ do
    it "" $ do
      let t = Type.Variable (TypeIndex (Kind.Variable (KindIndex 1)) 1) :: Type TypeIndex (Kind KindIndex)
      applyKindSub (1 `mapsToKind` Kind.Type) t == Type.Variable (TypeIndex Kind.Type 1)
    it "" $ do
      let t = Type.Variable (TypeIndex (Kind.Variable (KindIndex 1)) 1) :: Type TypeIndex (Kind KindIndex)
      applyKindSub (2 `mapsToKind` Kind.Type) t == t
    it "" $
      applyKindSub (3 `mapsToKind` Kind.Type) fixture_1
        == ( Expr.Let
              ( Binding.Pattern
                  (Pattern.Variable (Label (Type.Variable (TypeIndex Kind.Type 3) `Type.Arrow` Type.Variable (TypeIndex Kind.Type 3)) "f"))
                  ( Expr.Lambda
                      (Pattern.Variable (Label (Type.Variable (TypeIndex Kind.Type 3)) "x") :| [])
                      (Expr.Variable (Label (Type.Variable (TypeIndex Kind.Type 3)) "x"))
                  )
                  :| []
              )
              ( Expr.Application
                  (Type.Intrinsic Intrinsic.Int32)
                  ( Expr.Application
                      (Type.Intrinsic Intrinsic.Int32 `Type.Arrow` Type.Intrinsic Intrinsic.Int32)
                      (Expr.Variable (Label ((Type.Intrinsic Intrinsic.Int32 `Type.Arrow` Type.Intrinsic Intrinsic.Int32) `Type.Arrow` Type.Intrinsic Intrinsic.Int32 `Type.Arrow` Type.Intrinsic Intrinsic.Int32) "f"))
                      (Expr.Variable (Label (Type.Intrinsic Intrinsic.Int32 `Type.Arrow` Type.Intrinsic Intrinsic.Int32) "f") :| [])
                  )
                  ( Expr.Application
                      (Type.Intrinsic Intrinsic.Int32)
                      (Expr.Variable (Label (Type.Intrinsic Intrinsic.Int32 `Type.Arrow` Type.Intrinsic Intrinsic.Int32) "f"))
                      (Expr.Literal (Prim.Int32 1) :| [])
                      :| []
                  )
              )
           )

fixture_1 :: Expression (Type TypeIndex (Kind KindIndex))
fixture_1 =
  Expr.Let
    ( Binding.Pattern
        (Pattern.Variable (Label (Type.Variable (TypeIndex (Kind.Variable (KindIndex 3)) 3) `Type.Arrow` Type.Variable (TypeIndex (Kind.Variable (KindIndex 3)) 3)) "f"))
        ( Expr.Lambda
            (Pattern.Variable (Label (Type.Variable (TypeIndex (Kind.Variable (KindIndex 3)) 3)) "x") :| [])
            (Expr.Variable (Label (Type.Variable (TypeIndex (Kind.Variable (KindIndex 3)) 3)) "x"))
        )
        :| []
    )
    ( Expr.Application
        (Type.Intrinsic Intrinsic.Int32)
        ( Expr.Application
            (Type.Intrinsic Intrinsic.Int32 `Type.Arrow` Type.Intrinsic Intrinsic.Int32)
            (Expr.Variable (Label ((Type.Intrinsic Intrinsic.Int32 `Type.Arrow` Type.Intrinsic Intrinsic.Int32) `Type.Arrow` Type.Intrinsic Intrinsic.Int32 `Type.Arrow` Type.Intrinsic Intrinsic.Int32) "f"))
            (Expr.Variable (Label (Type.Intrinsic Intrinsic.Int32 `Type.Arrow` Type.Intrinsic Intrinsic.Int32) "f") :| [])
        )
        ( Expr.Application
            (Type.Intrinsic Intrinsic.Int32)
            (Expr.Variable (Label (Type.Intrinsic Intrinsic.Int32 `Type.Arrow` Type.Intrinsic Intrinsic.Int32) "f"))
            (Expr.Literal (Prim.Int32 1) :| [])
            :| []
        )
    )
