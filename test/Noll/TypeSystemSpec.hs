{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Noll.TypeSystemSpec where

import Control.Monad.State (evalState)
import Data.List.NonEmpty (NonEmpty (..))
import Noll.Label (Label (..))
import Noll.Language (Binding (..), Expression (..), Intrinsic (..), Kind (..), KindIndex (..), Pattern (..), Primitive (..), Type (..), TypeIndex (..), freshIdIn)
import Noll.Library.Supply (supply)
import Noll.TypeSystem.ConstraintSolver (SolverError (..), evalSolver, solveKinds, solveTypes)
import Noll.TypeSystem.KindConstraint (KindConstraint (..), KindConstraintMetadata (..))
import Noll.TypeSystem.KindConstraint.Collect (collectKindConstraints, runCollectKindConstraints)
import Noll.TypeSystem.KindSubstitution (KindSubstitution (..), applyKindSub)
import Noll.TypeSystem.TypeConstraint (TypeConstraint (..), TypeConstraintMetadata (..))
import Noll.TypeSystem.TypeConstraint.Collect (TypeConstraintsContext (..), collectTypeConstraints, evalCollectTypeConstraints)
import Noll.TypeSystem.TypeSubstitution (TypeSubstitutable (..), TypeSubstitution, normalizeTypeIndexes)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem" $ do
    it "" $
      validateResult fixture1 == fixture1Typed
    it "" $
      validateResult fixture2 == fixture2Typed
    it "" $
      typeErrorsInclude fixture4 (SolverError (ConstraintIfCondition "if"))
    it "" $
      typeErrorsInclude
        fixture5
        (SolverError (ConstraintIfBranches "if" (TIntrinsic IInt32) (TIntrinsic IBool)))
    it "" $
      typeErrorsIncludeAll
        fixture6
        [ SolverError (ConstraintIfBranches "if-2" (TIntrinsic IBool) (TIntrinsic IInt32))
        , SolverError (ConstraintIfCondition "if-1")
        ]

typeErrorsIncludeAll :: (Eq a) => Expression a () -> [SolverError (TypeConstraintMetadata (Kind KindIndex) a)] -> Bool
typeErrorsIncludeAll e = all (typeErrorsInclude e)

typeErrorsInclude :: (Eq a) => Expression a () -> SolverError (TypeConstraintMetadata (Kind KindIndex) a) -> Bool
typeErrorsInclude e err = err `elem` errs
 where
  (_, errs, _) = testInferTypes e

kindErrorsInclude :: (Eq a) => Expression a () -> SolverError KindConstraintMetadata -> Bool
kindErrorsInclude e err = err `elem` errs
 where
  (_, _, errs) = testInferTypes e

validateResult :: Expression () () -> Expression () (Type TypeIndex (Kind KindIndex))
validateResult e = let (a, _, _) = testInferTypes e in a

type Result a =
  ( Expression a (Type TypeIndex (Kind KindIndex))
  , [SolverError (TypeConstraintMetadata (Kind KindIndex) a)]
  , [SolverError KindConstraintMetadata]
  )

testInferTypes :: forall a. (Eq a) => Expression a () -> Result a
testInferTypes e =
  let
    e0 :: Expression a Int
    e0 = evalState (traverse (const supply) e) (0 :: Int)

    e1 :: Expression a (Type TypeIndex (Kind KindIndex))
    e1 = fmap typeVariable e0

    typeConstraints :: [TypeConstraint (TypeConstraintMetadata (Kind KindIndex) a) TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))]
    typeConstraints =
      evalCollectTypeConstraints
        (TypeConstraintsContext mempty mempty)
        (collectTypeConstraints e1)

    res1 :: (TypeSubstitution, [SolverError (TypeConstraintMetadata (Kind KindIndex) a)])
    res1 = evalSolver (freshIdIn typeConstraints) (solveTypes typeConstraints)

    (typeSub, errs1) = res1

    e2 :: Expression a (Type TypeIndex (Kind KindIndex))
    e2 = apply typeSub e1

    kindConstraints :: [KindConstraint KindConstraintMetadata (Kind KindIndex)]
    kindConstraints = runCollectKindConstraints mempty (collectKindConstraints e2)

    res2 :: (KindSubstitution, [SolverError KindConstraintMetadata])
    res2 = evalSolver 0 (solveKinds kindConstraints)

    (kindSub, errs2) = res2

    e3 :: Expression a (Type TypeIndex (Kind KindIndex))
    e3 = applyKindSub kindSub e2
   in
    (normalizeTypeIndexes e3, errs1, errs2)

typeVariable :: Int -> Type TypeIndex (Kind KindIndex)
typeVariable n = TVariable (TypeIndex (KVariable (KindIndex n)) n)

-- fn(m) => let y = m in let x = y(true) in x
fixture1 :: Expression () ()
fixture1 =
  ELambda
    ()
    (PVariable () (Label () "m") :| [])
    ( ELet
        ()
        ( BPattern
            (PVariable () (Label () "y"))
            (EVariable () (Label () "m"))
            :| []
        )
        ( ELet
            ()
            ( BPattern
                (PVariable () (Label () "x"))
                ( EApplication
                    ()
                    ()
                    (EVariable () (Label () "y"))
                    (ELiteral () (LBool True) :| [])
                )
                :| []
            )
            ( EVariable () (Label () "x")
            )
        )
    )

fixture1Typed :: Expression () (Type TypeIndex (Kind KindIndex))
fixture1Typed =
  ( ELambda
      ()
      (PVariable () (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex KType 0)) "m") :| [])
      ( ELet
          ()
          ( BPattern
              (PVariable () (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex KType 0)) "y"))
              (EVariable () (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex KType 0)) "m"))
              :| []
          )
          ( ELet
              ()
              ( BPattern
                  (PVariable () (Label (TVariable (TypeIndex KType 0)) "x"))
                  ( EApplication
                      ()
                      (TVariable (TypeIndex KType 0))
                      (EVariable () (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex KType 0)) "y"))
                      (ELiteral () (LBool True) :| [])
                  )
                  :| []
              )
              ( EVariable () (Label (TVariable (TypeIndex KType 0)) "x")
              )
          )
      )
  )

-- let f = fn(x) => x in (f(f))(f(1))
fixture2 :: Expression () ()
fixture2 =
  ELet
    ()
    ( BPattern
        (PVariable () (Label () "f"))
        ( ELambda
            ()
            (PVariable () (Label () "x") :| [])
            (EVariable () (Label () "x"))
        )
        :| []
    )
    ( EApplication
        ()
        ()
        ( EApplication
            ()
            ()
            (EVariable () (Label () "f"))
            (EVariable () (Label () "f") :| [])
        )
        ( EApplication
            ()
            ()
            (EVariable () (Label () "f"))
            (ELiteral () (LInt32 1) :| [])
            :| []
        )
    )

fixture2Typed :: Expression () (Type TypeIndex (Kind KindIndex))
fixture2Typed =
  ( ELet
      ()
      ( BPattern
          (PVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0)) "f"))
          ( ELambda
              ()
              (PVariable () (Label (TVariable (TypeIndex KType 0)) "x") :| [])
              (EVariable () (Label (TVariable (TypeIndex KType 0)) "x"))
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
  )

-- let x = 1 in x(x)
fixture3 :: Expression () ()
fixture3 =
  ELet
    ()
    ( BPattern
        (PVariable () (Label () "x"))
        (ELiteral () (LInt32 1))
        :| []
    )
    ( EApplication
        ()
        ()
        (EVariable () (Label () "x"))
        (EVariable () (Label () "x") :| [])
    )

-- if 1 then 2 else 3
fixture4 :: Expression String ()
fixture4 =
  EIf
    "if"
    ()
    (ELiteral "b" (LInt32 1))
    (ELiteral "c" (LInt32 2))
    (ELiteral "d" (LInt32 3))

-- if true then 2 else false
fixture5 :: Expression String ()
fixture5 =
  EIf
    "if"
    ()
    (ELiteral "b" (LBool True))
    (ELiteral "c" (LInt32 2))
    (ELiteral "d" (LBool False))

-- if 1 then 2 else (if true then false else 2)
fixture6 :: Expression String ()
fixture6 =
  EIf
    "if-1"
    ()
    (ELiteral "b" (LInt32 1))
    (ELiteral "c" (LInt32 2))
    ( EIf
        "if-2"
        ()
        (ELiteral "d" (LBool True))
        (ELiteral "f" (LBool False))
        (ELiteral "e" (LInt32 2))
    )
