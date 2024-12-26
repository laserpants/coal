{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemSpec where

import Control.Monad.State (evalState)
import Data.List.NonEmpty (NonEmpty (..))
import Noll.Label (Label (..))
import Noll.Language (Expression, Kind, KindIndex (..), Type, TypeIndex (..), freshIdIn)
import qualified Noll.Language.Expression as Expr
import qualified Noll.Language.Expression.Binding as Binding
import qualified Noll.Language.Pattern as Pattern
import qualified Noll.Language.Primitive as Prim
import qualified Noll.Language.Type as Type
import qualified Noll.Language.Type.Intrinsic as Intrinsic
import qualified Noll.Language.Type.Kind as Kind
import Noll.Library.Supply (supply)
import Noll.TypeSystem.KindConstraint (KindConstraint (..))
import Noll.TypeSystem.KindConstraint.Collect (collectKindConstraints, runCollectKindConstraints)
import Noll.TypeSystem.KindConstraint.Solver (solveKinds)
import Noll.TypeSystem.KindSubstitution (KindSubstitution (..), applyKindSub)
import Noll.TypeSystem.TypeConstraint (TypeConstraint (..))
import Noll.TypeSystem.TypeConstraint.Collect
import Noll.TypeSystem.TypeConstraint.Solver (solveTypes)
import Noll.TypeSystem.TypeSubstitution (TypeSubstitutable (..), TypeSubstitution, normalizeTypeIndexes)
import Noll.TypeSystem.Unifier (evalUnifier)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem" $ do
    it "" $
      spock fixture_1
        == ( Expr.Lambda
              (Pattern.Variable (Label (Type.Intrinsic Intrinsic.Bool `Type.Arrow` Type.Variable (TypeIndex Kind.Type 0)) "m") :| [])
              ( Expr.Let
                  ( Binding.Pattern
                      (Pattern.Variable (Label (Type.Intrinsic Intrinsic.Bool `Type.Arrow` Type.Variable (TypeIndex Kind.Type 0)) "y"))
                      (Expr.Variable (Label (Type.Intrinsic Intrinsic.Bool `Type.Arrow` Type.Variable (TypeIndex Kind.Type 0)) "m"))
                      :| []
                  )
                  ( Expr.Let
                      ( Binding.Pattern
                          (Pattern.Variable (Label (Type.Variable (TypeIndex Kind.Type 0)) "x"))
                          ( Expr.Application
                              (Type.Variable (TypeIndex Kind.Type 0))
                              (Expr.Variable (Label (Type.Intrinsic Intrinsic.Bool `Type.Arrow` Type.Variable (TypeIndex Kind.Type 0)) "y"))
                              (Expr.Literal (Prim.Bool True) :| [])
                          )
                          :| []
                      )
                      ( Expr.Variable (Label (Type.Variable (TypeIndex Kind.Type 0)) "x")
                      )
                  )
              )
           )
    it "" $
      spock fixture_2
        == ( Expr.Let
              ( Binding.Pattern
                  (Pattern.Variable (Label (Type.Variable (TypeIndex Kind.Type 0) `Type.Arrow` Type.Variable (TypeIndex Kind.Type 0)) "f"))
                  ( Expr.Lambda
                      (Pattern.Variable (Label (Type.Variable (TypeIndex Kind.Type 0)) "x") :| [])
                      (Expr.Variable (Label (Type.Variable (TypeIndex Kind.Type 0)) "x"))
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

spock e = e4 -- undefined
 where
  e4 = normalizeTypeIndexes e3

  e3 :: Expression (Type TypeIndex (Kind KindIndex))
  e3 = applyKindSub kindSub e2

  kindSub :: KindSubstitution
  kindSub = evalState (solveKinds kindConstraints) 1

  kindConstraints :: [KindConstraint (Kind KindIndex)]
  kindConstraints = runCollectKindConstraints mempty (collectKindConstraints e2)

  e2 :: Expression (Type TypeIndex (Kind KindIndex))
  e2 = apply typeSub e1

  typeSub :: TypeSubstitution
  typeSub = evalUnifier (freshIdIn constraints) (solveTypes constraints)

  constraints :: [TypeConstraint TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))]
  constraints =
    evalCollectTypeConstraints
      (TypeConstraintsContext mempty mempty)
      (collectConstraints e1)

  e1 = fmap typeVariable e0
  e0 = evalState (traverse (const supply) e) (0 :: Int)

typeVariable :: Int -> Type TypeIndex (Kind KindIndex)
typeVariable n = Type.Variable (TypeIndex (Kind.Variable (KindIndex n)) n)

-- fn(m) => let y = m in let x = y(true) in x
fixture_1 :: Expression ()
fixture_1 =
  Expr.Lambda
    (Pattern.Variable (Label () "m") :| [])
    ( Expr.Let
        ( Binding.Pattern
            (Pattern.Variable (Label () "y"))
            (Expr.Variable (Label () "m"))
            :| []
        )
        ( Expr.Let
            ( Binding.Pattern
                (Pattern.Variable (Label () "x"))
                ( Expr.Application
                    ()
                    (Expr.Variable (Label () "y"))
                    (Expr.Literal (Prim.Bool True) :| [])
                )
                :| []
            )
            ( Expr.Variable (Label () "x")
            )
        )
    )

fixture_2 :: Expression ()
fixture_2 =
  Expr.Let
    ( Binding.Pattern
        (Pattern.Variable (Label () "f"))
        ( Expr.Lambda
            (Pattern.Variable (Label () "x") :| [])
            (Expr.Variable (Label () "x"))
        )
        :| []
    )
    ( Expr.Application
        ()
        ( Expr.Application
            ()
            (Expr.Variable (Label () "f"))
            (Expr.Variable (Label () "f") :| [])
        )
        ( Expr.Application
            ()
            (Expr.Variable (Label () "f"))
            (Expr.Literal (Prim.Int32 1) :| [])
            :| []
        )
    )
