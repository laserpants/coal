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
import Noll.Language.Type.Kind (Kind (..))
import qualified Noll.Language.Type.Kind as Kind
import Noll.Language.Type.Kind.Index (KindIndex (..))
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.KindConstraint.Collect" $ do
    it "" $
      1 == 2

typeVariable :: Int -> Type TypeIndex (Kind KindIndex)
typeVariable n = Type.Variable (TypeIndex (Kind.Variable (KindIndex n)) n)

-- let f = fn(x) => x in (f f)(f 1)
fixture_1 :: Expression Int
fixture_1 =
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
