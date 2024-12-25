{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.KindConstraint.CollectSpec where

import Data.List.NonEmpty (NonEmpty (..))
import Noll.Label (Label (..))
import Noll.Language.Expression (Expression)
import qualified Noll.Language.Expression as Expr
import qualified Noll.Language.Expression.Binding as Binding
import qualified Noll.Language.Pattern as Pattern
import qualified Noll.Language.Primitive as Primitive
import Noll.Language.Type (Type (..))
import qualified Noll.Language.Type as Type
import Noll.Language.Type.Index (TypeIndex (..))
import qualified Noll.Language.Type.Intrinsic as Intrinsic
import Noll.Language.Type.Kind (Kind (..))
import qualified Noll.Language.Type.Kind as Kind
import Noll.Language.Type.Kind.Index (KindIndex (..))
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.KindConstraint.Collect" $ do
    it "" $
      1 == 2

-- let f = fn(x) => x in (f f)(f 1)
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
            (Expr.Literal (Primitive.Int32 1) :| [])
            :| []
        )
    )
