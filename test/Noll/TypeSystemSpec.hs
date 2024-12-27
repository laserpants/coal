{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Noll.TypeSystem.TypeConstraint.Collect (TypeConstraintsContext (..), collectConstraints, evalCollectTypeConstraints)
import Noll.TypeSystem.TypeSubstitution (TypeSubstitutable (..), TypeSubstitution, normalizeTypeIndexes)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem" $ do
    it "" $ hasTypedExpression fixture1 == fixture1Typed
    it "" $ hasTypedExpression fixture2 == fixture2Typed
    it "" $ hasSolverTypeError fixture4 (SolverError (ConstraintIfCondition "if"))
    it "" $ hasSolverTypeError fixture5 (SolverError (ConstraintIfBranches "if" (TIntrinsic IInt32) (TIntrinsic IBool)))

hasSolverTypeError :: (Eq a) => Expression a () -> SolverError (TypeConstraintMetadata (Kind KindIndex) a) -> Bool
hasSolverTypeError e err = let (_, errs, _) = addTypes e in err `elem` errs

hasSolverKindError :: (Eq a) => Expression a () -> SolverError KindConstraintMetadata -> Bool
hasSolverKindError e err = let (_, _, errs) = addTypes e in err `elem` errs

hasTypedExpression :: Expression () () -> Expression () (Type TypeIndex (Kind KindIndex))
hasTypedExpression e = let (a, _, _) = addTypes e in a

addTypes ::
  (Eq a) =>
  Expression a () ->
  ( Expression a (Type TypeIndex (Kind KindIndex))
  , [SolverError (TypeConstraintMetadata (Kind KindIndex) a)]
  , [SolverError KindConstraintMetadata]
  )
addTypes e = (normalizeTypeIndexes e3, errs1, errs2)
 where
  --  e3 :: Expression () (Type TypeIndex (Kind KindIndex))
  e3 = applyKindSub kindSub e2

  (kindSub, errs2) = res2

  -- TODO
  --  res2 :: (KindSubstitution, [SolverError KindConstraintMetadata])
  res2 = evalSolver 0 (solveKinds kindConstraints)

  --  kindConstraints :: [KindConstraint KindConstraintMetadata (Kind KindIndex)]
  kindConstraints = runCollectKindConstraints mempty (collectKindConstraints e2)

  --  e2 :: Expression () (Type TypeIndex (Kind KindIndex))
  e2 = apply typeSub e1

  (typeSub, errs1) = res1

  --  res1 :: (TypeSubstitution, [SolverError (TypeConstraintMetadata ())])
  res1 = evalSolver (freshIdIn typeConstraints) (solveTypes typeConstraints)

  --  typeConstraints :: [TypeConstraint (TypeConstraintMetadata ()) TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))]
  typeConstraints =
    evalCollectTypeConstraints
      (TypeConstraintsContext mempty mempty)
      (collectConstraints e1)

  --  e1 :: Expression () (Type TypeIndex (Kind KindIndex))
  e1 = fmap typeVariable e0

  --  e0 :: Expression () Int
  e0 = evalState (traverse (const supply) e) (0 :: Int)

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

-- let f = fn(x) => x in (f f)(f 1)
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
    (ELiteral "b" (LInt32 1))
    (ELiteral "c" (LInt32 2))
    (ELiteral "d" (LInt32 3))

-- if true then 2 else false
fixture5 :: Expression String ()
fixture5 =
  EIf
    "if"
    (ELiteral "b" (LBool True))
    (ELiteral "c" (LInt32 2))
    (ELiteral "d" (LBool False))

fixture6 :: Expression String ()
fixture6 =
  EIf
    "if-1"
    (ELiteral "b" (LInt32 1))
    (ELiteral "c" (LInt32 2))
    ( EIf
        "if-2"
        (ELiteral "d" (LBool True))
        (ELiteral "f" (LBool False))
        (ELiteral "e" (LInt32 2))
    )
