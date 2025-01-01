{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Noll.TypeSystem.KindConstraint.CollectSpec where -- (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Noll.Label (Label (..))
import Noll.Language (Binding (..), Expression (..), Intrinsic (..), Kind (..), KindIndex (..), Pattern (..), Primitive (..), Type (..), TypeIndex (..), freshIdIn)
import Noll.TypeSystem.KindConstraint (KindConstraint (..))
import Noll.TypeSystem.KindConstraint.Collect (KindCollectError (..), collectKindConstraints, runCollectKindConstraints)
import Noll.TypeSystem.KindConstraint.Rule (KindRule (..))
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.KindConstraint.Collect" $ do
    describe "collectKindConstraints" $ do
      it "" $
        kindConstraintsIncludeAll
          fixture1
          [ KindEquality KindRule (KVariable (KindIndex 3)) KType
          ]
      it "" $
        kindConstraintsIncludeAll
          fixture2
          [ KindEquality KindRule (KVariable (KindIndex 3)) KType
          ]
      it "" $
        hasError fixture3 (MissingTypeConstructor "Nope")

kindConstraintsIncludeAll :: Expression a (Type TypeIndex ()) -> [KindConstraint KindRule (Kind KindIndex)] -> Bool
kindConstraintsIncludeAll = all . kindConstraintsInclude

kindConstraintsInclude :: forall a. Expression a (Type TypeIndex ()) -> KindConstraint KindRule (Kind KindIndex) -> Bool
kindConstraintsInclude e =
  \case
    KindEquality meta k1 k2 ->
      elem (KindEquality meta k1 k2) constraints || elem (KindEquality meta k2 k1) constraints
 where
  (_, _, constraints) = getResultsFor e

hasError :: forall a. Expression a (Type TypeIndex ()) -> KindCollectError a -> Bool
hasError e err = err `elem` errs
 where
  (_, errs, _) = getResultsFor e

getResultsFor :: Expression a (Type TypeIndex ()) -> (Expression a (Type TypeIndex (Kind KindIndex)), [KindCollectError a], [KindConstraint KindRule (Kind KindIndex)])
getResultsFor e = runCollectKindConstraints mempty (freshIdIn e) (collectKindConstraints e)

-- fn(m) => let y = m in let x = y(true) in x
fixture1 :: Expression () (Type TypeIndex ())
fixture1 =
  ELambda
    ()
    (PVariable () (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex () 3)) "m") :| [])
    ( ELet
        ()
        ( BPattern
            ()
            (PVariable () (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex () 3)) "y"))
            (EVariable () (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex () 3)) "m"))
            :| []
        )
        ( ELet
            ()
            ( BPattern
                ()
                (PVariable () (Label (TVariable (TypeIndex () 3)) "x"))
                ( EApplication
                    ()
                    (TVariable (TypeIndex () 3))
                    (EVariable () (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex () 3)) "y"))
                    (ELiteral () (LBool True) :| [])
                )
                :| []
            )
            ( EVariable () (Label (TVariable (TypeIndex () 3)) "x")
            )
        )
    )

-- let f = fn(x) => x in (f(f))(f(1))
fixture2 :: Expression () (Type TypeIndex ())
fixture2 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label (TVariable (TypeIndex () 3) `TArrow` TVariable (TypeIndex () 3)) "f"))
        ( ELambda
            ()
            (PVariable () (Label (TVariable (TypeIndex () 3)) "x") :| [])
            (EVariable () (Label (TVariable (TypeIndex () 3)) "x"))
        )
        :| []
    )
    ( EApplication
        ()
        (TIntrinsic IInt32)
        ( EApplication
            ()
            (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
            (EVariable () (Label ((TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f"))
            (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f") :| [])
        )
        ( EApplication
            ()
            (TIntrinsic IInt32)
            (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f"))
            (ELiteral () (LInt32 1) :| [])
            :| []
        )
    )

-- x : Nope('0)
fixture3 :: Expression () (Type TypeIndex ())
fixture3 = EVariable () (Label (TApplication () (TConstructor () "Nope") (TVariable (TypeIndex () 0) :| [])) "x")
