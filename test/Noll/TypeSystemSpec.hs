{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemSpec where

import Control.Monad.State (evalState)
import Data.Either.Extra (lefts, rights)
import qualified Data.Set as Set
import qualified Noll.Library.Environment as Environment
import Noll.Library.Environment (Environment)
import Noll.Label (Label (..))
import Noll.Language (Binding (..), Constructor (..), Scheme (..), Clause (..), Choice (..), Expression (..), IndexedType, Intrinsic (..), Kind (..), Pattern (..), Primitive (..), Type (..), TypeIndex (..))
import Data.List.NonEmpty ((<|))
import Noll.Library.List1 (NonEmpty (..))
import Noll.Library.Supply (supply)
import Noll.TypeSystem.Constraint.Aggregation
import Noll.TypeSystem.Constraint.Rule (Assumption (..), InferenceRule (..))
import Noll.TypeSystem.Constraint.Solver (solveConstraints)
import Noll.TypeSystem.Substitution (apply, normalizeTypeIndexes)
import Test.Hspec (Spec, describe, hspec, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem" $ do
    it "fn(m) => let y = m in let x = y(true) in x" $
      validateResult fixture1 == fixture1Typed
    it "let f = fn(x) => x in (f(f))(f(1))" $
      validateResult fixture2 == fixture2Typed
    it "" $
      validateResult fixture7 == fixture7Typed

-- validateSolverErrors :: Expression () () -> Expression () (Type TypeIndex Kind)
-- validateSolverErrors ::

validateResult :: Expression () () -> Expression () (Type TypeIndex Kind)
validateResult e = e1
 where
  (e1, _, _, _) = testRunner e

--testRunner ::
--  Expression () () ->
--  ( Expression () (Type TypeIndex Kind)
--  , [Assumption IndexedType]
--  , [AggregationError ()]
--  , [InferenceRule Kind ()]
--  )
testRunner e =
  let
    e0 = evalState (traverse (const supply) e) (0 :: Int)

    e1 = toIndexed e0

    (asms, out) =
      runAggregationStack
        (AggregationContext testConstructorEnv mempty mempty)
        (aggregateConstraints e1)

    errors0 = lefts out
    constraints = rights out

    res0 = solveConstraints constraints

    (sub, errors1) = res0

    e2 = apply sub e1

    e3 = normalizeTypeIndexes e2
   in
    (e3, asms, errors0, apply sub errors1)

toIndexed :: Expression a Int -> Expression a (Type TypeIndex Kind)
toIndexed = fmap (TVariable . TypeIndex KType)

testConstructorEnv :: Environment (Constructor TypeIndex Kind (Type TypeIndex Kind))
testConstructorEnv =
  Environment.fromList
    [
--      ( "Yes"
--      , Constructor "Yes" 0 (Forall mempty [] (TConstructor () "Answer"))
--      )
--    ,
--      ( "No"
--      , Constructor "No" 0 (Forall mempty [] (TConstructor () "Answer"))
--      )
--    ,
--      ( "Foo"
--      , Constructor "Foo" 0 (Forall mempty [] (TConstructor () "Foo"))
--      )
--    ,
--      ( "Id"
--      , Constructor
--          "Id"
--          1
--          ( Forall (Set.fromList [TypeIndex () 0]) [] (TVariable (TypeIndex () 0) `TArrow` TApplication () (TConstructor () "Id") (TVariable (TypeIndex () 0) :| []))
--          )
--      )
--    ,
--      ( "MkPair1"
--      , Constructor
--          "MkPair1"
--          2
--          ( Forall
--              (Set.fromList [TypeIndex () 0])
--              []
--              ( TVariable (TypeIndex () 0)
--                  `TArrow` TVariable (TypeIndex () 0)
--                  `TArrow` TApplication -- '0 -> Pair1('0)
--                    ()
--                    (TConstructor () "Pair1")
--                    (TVariable (TypeIndex () 0) :| [])
--              )
--          )
--      )
--    ,
--      ( "MkPair"
--      , Constructor
--          "MkPair"
--          2
--          ( Forall
--              (Set.fromList [TypeIndex () 0, TypeIndex () 1])
--              []
--              ( TVariable (TypeIndex () 0)
--                  `TArrow` TVariable (TypeIndex () 1)
--                  `TArrow` TApplication -- '0 -> '1 -> Pair('0, '1)
--                    ()
--                    (TConstructor () "Pair")
--                    ( TVariable (TypeIndex () 0)
--                        <| TVariable (TypeIndex () 1)
--                          :| []
--                    )
--              )
--          )
--      )
--    ,
--      ( "MkIntPair"
--      , Constructor
--          "MkIntPair"
--          2
--          ( Forall
--              mempty
--              []
--              (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TConstructor () "IntPair")
--          )
--      )
    ]


-- fn(m) => let y = m in let x = y(true) in x
fixture1 :: Expression () ()
fixture1 =
  ELambda
    ()
    (PVariable () (Label () "m") :| [])
    ( ELet
        ()
        ( BPattern
            ()
            (PVariable () (Label () "y"))
            (EVariable () (Label () "m"))
            :| []
        )
        ( ELet
            ()
            ( BPattern
                ()
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

fixture1Typed :: Expression () (Type TypeIndex Kind)
fixture1Typed =
  ( ELambda
      ()
      (PVariable () (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex KType 0)) "m") :| [])
      ( ELet
          ()
          ( BPattern
              ()
              (PVariable () (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex KType 0)) "y"))
              (EVariable () (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex KType 0)) "m"))
              :| []
          )
          ( ELet
              ()
              ( BPattern
                  ()
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
        ()
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

fixture2Typed :: Expression () (Type TypeIndex Kind)
fixture2Typed =
  ( ELet
      ()
      ( BPattern
          ()
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

-- TODO
-- let x = 1 in x(x)
fixture3 :: Expression () ()
fixture3 =
  ELet
    ()
    ( BPattern
        ()
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

-- TODO
-- if 1 then 2 else 3
fixture4 :: Expression String ()
fixture4 =
  EIf
    "if"
    ()
    (ELiteral "b" (LInt32 1))
    (ELiteral "c" (LInt32 2))
    (ELiteral "d" (LInt32 3))

-- TODO
-- if true then 2 else false
fixture5 :: Expression String ()
fixture5 =
  EIf
    "if"
    ()
    (ELiteral "b" (LBool True))
    (ELiteral "c" (LInt32 2))
    (ELiteral "d" (LBool False))

-- TODO
-- if 1 then 2 else (if true then false else 2)
fixture6 :: Expression String ()
fixture6 =
  EIf
    "EIf-1"
    ()
    (ELiteral "ELiteral-1" (LInt32 1))
    (ELiteral "ELiteral-2" (LInt32 2))
    ( EIf
        "EIf-2"
        ()
        (ELiteral "ELiteral-3" (LBool True))
        (ELiteral "ELiteral-4" (LBool False))
        (ELiteral "ELiteral-5" (LInt32 2))
    )

-- match x { | Yes => true }
fixture7 :: Expression () ()
fixture7 =
  ( EMatch
      ()
      ()
      (EVariable () (Label () "x"))
      ( EClause
          ()
          (PConstructor () (Label () "Yes") [])
          (CPlain () [] (ELiteral () (LBool True)) :| [])
          :| []
      )
  )

fixture7Typed :: Expression () (Type TypeIndex Kind)
fixture7Typed =
  ( EMatch
      ()
      (TIntrinsic IBool)
      (EVariable () (Label (TConstructor KType "Answer") "x"))
      ( EClause
          ()
          (PConstructor () (Label (TConstructor KType "Answer") "Yes") [])
          (CPlain () [] (ELiteral () (LBool True)) :| [])
          :| []
      )
  )
