{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemSpec where

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
    it "" $
      spock fixture_1
        == ( ELambda
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
                              (ELiteral (ABool True) :| [])
                          )
                          :| []
                      )
                      ( EVariable (Label (TVariable (TypeIndex KType 0)) "x")
                      )
                  )
              )
           )
    it "" $
      spock fixture_2
        == ( ELet
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
                      (ELiteral (AInt32 1) :| [])
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
typeVariable n = TVariable (TypeIndex (KVariable (KindIndex n)) n)

-- fn(m) => let y = m in let x = y(true) in x
fixture_1 :: Expression ()
fixture_1 =
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
                    (ELiteral (ABool True) :| [])
                )
                :| []
            )
            ( EVariable (Label () "x")
            )
        )
    )

-- let f = fn(x) => x in (f f)(f 1)
fixture_2 :: Expression ()
fixture_2 =
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
            (ELiteral (AInt32 1) :| [])
            :| []
        )
    )
