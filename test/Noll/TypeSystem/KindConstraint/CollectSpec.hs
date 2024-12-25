{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.KindConstraint.CollectSpec where

import Control.Monad.Writer (execWriter)
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
import Noll.TypeSystem.KindConstraint (KindConstraint (..))
import Noll.TypeSystem.KindConstraint.Collect (collectKindConstraints, runCollectKindConstraints)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.KindConstraint.Collect" $ do
    it "" $
      hasConstraints
        fixture_1
        [ KindEquality (Kind.Variable (KindIndex 3)) Kind.Type
        ]
    it "" $
      hasConstraints
        fixture_2
        [ KindEquality (Kind.Variable (KindIndex 3)) Kind.Type
        ]

hasConstraints :: Expression (Type TypeIndex (Kind KindIndex)) -> [KindConstraint (Kind KindIndex)] -> Bool
hasConstraints = all . hasConstraint

hasConstraint :: Expression (Type TypeIndex (Kind KindIndex)) -> KindConstraint (Kind KindIndex) -> Bool
hasConstraint e =
  \case
    KindEquality k1 k2 ->
      elem (KindEquality k1 k2) constraints || elem (KindEquality k2 k1) constraints
 where
  constraints = runCollectKindConstraints mempty (collectKindConstraints e)

-- fn(m) => let y = m in let x = y(true) in x
fixture_1 :: Expression (Type TypeIndex (Kind KindIndex))
fixture_1 =
  Expr.Lambda
    (Pattern.Variable (Label (Type.Intrinsic Intrinsic.Bool `Type.Arrow` Type.Variable (TypeIndex (Kind.Variable (KindIndex 3)) 3)) "m") :| [])
    ( Expr.Let
        ( Binding.Pattern
            (Pattern.Variable (Label (Type.Intrinsic Intrinsic.Bool `Type.Arrow` Type.Variable (TypeIndex (Kind.Variable (KindIndex 3)) 3)) "y"))
            (Expr.Variable (Label (Type.Intrinsic Intrinsic.Bool `Type.Arrow` Type.Variable (TypeIndex (Kind.Variable (KindIndex 3)) 3)) "m"))
            :| []
        )
        ( Expr.Let
            ( Binding.Pattern
                (Pattern.Variable (Label (Type.Variable (TypeIndex (Kind.Variable (KindIndex 3)) 3)) "x"))
                ( Expr.Application
                    (Type.Variable (TypeIndex (Kind.Variable (KindIndex 3)) 3))
                    (Expr.Variable (Label (Type.Intrinsic Intrinsic.Bool `Type.Arrow` Type.Variable (TypeIndex (Kind.Variable (KindIndex 3)) 3)) "y"))
                    (Expr.Literal (Primitive.Bool True) :| [])
                )
                :| []
            )
            ( Expr.Variable (Label (Type.Variable (TypeIndex (Kind.Variable (KindIndex 3)) 3)) "x")
            )
        )
    )

-- let f = fn(x) => x in (f f)(f 1)
fixture_2 :: Expression (Type TypeIndex (Kind KindIndex))
fixture_2 =
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
