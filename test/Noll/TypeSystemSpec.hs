{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemSpec where

import Control.Monad.Identity (runIdentity)
import Control.Monad.State (evalState)
import Data.List.NonEmpty (NonEmpty (..))
import Noll.Label (Label (..))
import Noll.Language (Binding (..), Expression (..), Intrinsic (..), Kind (..), KindIndex (..), Pattern (..), Primitive (..), Type (..), TypeIndex (..), freshIdIn)
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
    it "" $ testAddTypes fixture1 == fixture1Typed
    it "" $ testAddTypes fixture2 == fixture2Typed

testAddTypes :: Expression () -> Expression (Type TypeIndex (Kind KindIndex))
testAddTypes e = normalizeTypeIndexes e3
 where
  e3 :: Expression (Type TypeIndex (Kind KindIndex))
  e3 = applyKindSub kindSub e2

  kindSub :: KindSubstitution
  kindSub = runIdentity (solveKinds kindConstraints)

  kindConstraints :: [KindConstraint (Kind KindIndex)]
  kindConstraints = runCollectKindConstraints mempty (collectKindConstraints e2)

  e2 :: Expression (Type TypeIndex (Kind KindIndex))
  e2 = apply typeSub e1

  typeSub :: TypeSubstitution
  typeSub = evalUnifier (freshIdIn typeConstraints) (solveTypes typeConstraints)

  typeConstraints :: [TypeConstraint TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))]
  typeConstraints =
    evalCollectTypeConstraints
      (TypeConstraintsContext mempty mempty)
      (collectConstraints e1)

  e1 :: Expression (Type TypeIndex (Kind KindIndex))
  e1 = fmap typeVariable e0

  e0 :: Expression Int
  e0 = evalState (traverse (const supply) e) (0 :: Int)

typeVariable :: Int -> Type TypeIndex (Kind KindIndex)
typeVariable n = TVariable (TypeIndex (KVariable (KindIndex n)) n)

-- fn(m) => let y = m in let x = y(true) in x
fixture1 :: Expression ()
fixture1 =
  ELambda
    (PVariable (Label () "m") :| [])
    ( ELet
        ( BPattern
            (PVariable (Label () "y"))
            (EVariable (Label () "m"))
            :| []
        )
        ( ELet
            ( BPattern
                (PVariable (Label () "x"))
                ( EApplication
                    ()
                    (EVariable (Label () "y"))
                    (ELiteral (LBool True) :| [])
                )
                :| []
            )
            ( EVariable (Label () "x")
            )
        )
    )

fixture1Typed :: Expression (Type TypeIndex (Kind KindIndex))
fixture1Typed =
  ( ELambda
      (PVariable (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex KType 0)) "m") :| [])
      ( ELet
          ( BPattern
              (PVariable (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex KType 0)) "y"))
              (EVariable (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex KType 0)) "m"))
              :| []
          )
          ( ELet
              ( BPattern
                  (PVariable (Label (TVariable (TypeIndex KType 0)) "x"))
                  ( EApplication
                      (TVariable (TypeIndex KType 0))
                      (EVariable (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex KType 0)) "y"))
                      (ELiteral (LBool True) :| [])
                  )
                  :| []
              )
              ( EVariable (Label (TVariable (TypeIndex KType 0)) "x")
              )
          )
      )
  )

-- let f = fn(x) => x in (f f)(f 1)
fixture2 :: Expression ()
fixture2 =
  ELet
    ( BPattern
        (PVariable (Label () "f"))
        ( ELambda
            (PVariable (Label () "x") :| [])
            (EVariable (Label () "x"))
        )
        :| []
    )
    ( EApplication
        ()
        ( EApplication
            ()
            (EVariable (Label () "f"))
            (EVariable (Label () "f") :| [])
        )
        ( EApplication
            ()
            (EVariable (Label () "f"))
            (ELiteral (LInt32 1) :| [])
            :| []
        )
    )

-- let x = 1 in x(x)
fixture3 :: Expression ()
fixture3 =
  ELet
    ( BPattern
        (PVariable (Label () "x"))
        (ELiteral (LInt32 1))
        :| []
    )
    (
      EApplication
        ()
        (EVariable (Label () "x"))
        (EVariable (Label () "x") :| [])
    )

fixture2Typed :: Expression (Type TypeIndex (Kind KindIndex))
fixture2Typed =
  ( ELet
      ( BPattern
          (PVariable (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0)) "f"))
          ( ELambda
              (PVariable (Label (TVariable (TypeIndex KType 0)) "x") :| [])
              (EVariable (Label (TVariable (TypeIndex KType 0)) "x"))
          )
          :| []
      )
      ( EApplication
          (TIntrinsic IInt32)
          ( EApplication
              (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
              (EVariable (Label ((TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f"))
              (EVariable (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f") :| [])
          )
          ( EApplication
              (TIntrinsic IInt32)
              (EVariable (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f"))
              (ELiteral (LInt32 1) :| [])
              :| []
          )
      )
  )
