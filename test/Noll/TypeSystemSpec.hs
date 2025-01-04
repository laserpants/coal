{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemSpec where

import Control.Monad.State (evalState)
import Data.Either.Extra (lefts, rights)
import Data.List.NonEmpty ((<|))
import qualified Data.Set as Set
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Choice (..),
  Clause (..),
  Constructor (..),
  Expression (..),
  IndexedType,
  Intrinsic (..),
  Kind (..),
  Pattern (..),
  Primitive (..),
  Scheme (..),
  Type (..),
  TypeIndex (..),
  TypeParam (..),
  freshIdIn,
 )
import Noll.Library.Environment (Environment)
import qualified Noll.Library.Environment as Environment
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
    it "fn(m) => let y = m in let x = y(true) in x" $ do
      validateResult fixture1 == fixture1Typed
    it "" $
      validateNoErrors fixture1
    it "let f = fn(x) => x in (f(f))(f(1))" $ do
      validateResult fixture2 == fixture2Typed
    it "" $
      validateNoErrors fixture2
    it "match x { | Yes => true }" $ do
      validateResult fixture7 == fixture7Typed
    it "" $
      validateNoErrors fixture7
    it "match(p) { | MkPair(fst, snd) => true }" $ do
      validateResult fixture12 == fixture12Typed
    it "" $
      validateNoErrors fixture12
    it "match(p : Pair(int32, bool)) { | MkPair(fst, snd) => true }" $ do
      validateResult fixture13 == fixture13Typed
    it "" $
      validateNoErrors fixture13
    it "match(p : Pair(a, b)) { | MkPair(fst, snd) => true }" $ do
      validateResult fixture14 == fixture14Typed
    it "" $
      validateNoErrors fixture14
    it "match(p : Pair(a, a)) { | MkPair(fst, snd) => true }" $ do
      validateResult fixture15 == fixture15Typed
    it "" $
      validateNoErrors fixture15
    it "" $ do
      validateResult fixture17 == fixture17Typed
    it "" $
      validateNoErrors fixture17

-- validateSolverErrors :: Expression () () -> Expression () (Type TypeIndex Kind)
-- validateSolverErrors ::

validateResult :: Expression () () -> Expression () (Type TypeIndex Kind)
validateResult e = e1
 where
  (e1, _, _, _) = testRunner e

validateNoErrors :: Expression () () -> Bool
validateNoErrors e = null es0 && null es1
 where
  (_, _, es0, es1) = testRunner e

testRunner ::
  Expression () () ->
  ( Expression () (Type TypeIndex Kind)
  , [Assumption IndexedType]
  , [AggregationError ()]
  , [InferenceRule Kind ()]
  )
testRunner e =
  let
    e0 = evalState (traverse (const supply) e) (0 :: Int)

    e1 = toIndexed e0

    (asms, out) =
      runAggregationStack
        (freshIdIn e1)
        (AggregationContext mempty testConstructorEnv testTypeConstructorEnv)
        (aggregateConstraints e1)

    errors0 = lefts out
    constraints = rights out

    res0 = solveConstraints constraints

    (sub, errors1) = res0

    e2 = apply sub e1

    e3 = normalizeTypeIndexes e2
   in
    ( e3
    , apply sub asms
    , errors0
    , apply sub errors1
    )

toIndexed :: Expression a Int -> Expression a (Type TypeIndex Kind)
toIndexed = fmap (TVariable . TypeIndex KType)

testConstructorEnv :: Environment (Constructor TypeIndex Kind (Type TypeIndex Kind))
testConstructorEnv =
  Environment.fromList
    [
      ( "Yes"
      , Constructor "Yes" 0 (Forall mempty [] (TConstructor KType "Answer"))
      )
    ,
      ( "No"
      , Constructor "No" 0 (Forall mempty [] (TConstructor KType "Answer"))
      )
    ,
      ( "Foo"
      , Constructor "Foo" 0 (Forall mempty [] (TConstructor KType "Foo"))
      )
    ,
      ( "Id"
      , Constructor
          "Id"
          1
          ( Forall (Set.fromList [TypeIndex KType 0]) [] (TVariable (TypeIndex KType 0) `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Id") (TVariable (TypeIndex KType 0) :| []))
          )
      )
    ,
      ( "MkPair1"
      , Constructor
          "MkPair1"
          2
          ( Forall
              (Set.fromList [TypeIndex KType 0])
              []
              ( TVariable (TypeIndex KType 0)
                  `TArrow` TVariable (TypeIndex KType 0)
                  `TArrow` TApplication -- '0 -> Pair1('0)
                    KType
                    (TConstructor (KArrow KType KType) "Pair1")
                    (TVariable (TypeIndex KType 0) :| [])
              )
          )
      )
    ,
      ( "MkPair"
      , Constructor
          "MkPair"
          2
          ( Forall
              (Set.fromList [TypeIndex KType 0, TypeIndex KType 1])
              []
              ( TVariable (TypeIndex KType 0)
                  `TArrow` TVariable (TypeIndex KType 1)
                  `TArrow` TApplication -- '0 -> '1 -> Pair('0, '1)
                    KType
                    (TConstructor (KArrow KType (KArrow KType KType)) "Pair")
                    ( TVariable (TypeIndex KType 0)
                        <| TVariable (TypeIndex KType 1)
                          :| []
                    )
              )
          )
      )
    ,
      ( "MkIntPair"
      , Constructor
          "MkIntPair"
          2
          ( Forall
              mempty
              []
              (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TConstructor KType "IntPair")
          )
      )
    ]

testTypeConstructorEnv :: Environment Kind
testTypeConstructorEnv =
  Environment.fromList
    [ ("Answer", KType)
    , ("Pair1", KArrow KType KType) -- Homogeneous pair type
    , ("Pair", KArrow KType (KArrow KType KType))
    , ("IntPair", KType)
    , ("Id", KArrow KType KType)
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

-- match(p) { | MkPair(fst, snd) => true }
fixture12 :: Expression () ()
fixture12 =
  ( EMatch
      ()
      ()
      (EVariable () (Label () "p"))
      ( EClause
          ()
          (PConstructor () (Label () "MkPair") [PVariable () (Label () "fst"), PVariable () (Label () "snd")])
          (CPlain () [] (ELiteral () (LBool True)) :| [])
          :| []
      )
  )

fixture12Typed :: Expression () (Type TypeIndex Kind)
fixture12Typed =
  ( EMatch
      ()
      (TIntrinsic IBool)
      ( EVariable
          ()
          ( Label
              ( TApplication
                  KType
                  (TConstructor (KArrow KType (KArrow KType KType)) "Pair")
                  (TVariable (TypeIndex KType 1) :| [TVariable (TypeIndex KType 0)])
              )
              "p"
          )
      )
      ( EClause
          ()
          ( PConstructor
              ()
              ( Label
                  ( TApplication
                      KType
                      (TConstructor (KArrow KType (KArrow KType KType)) "Pair")
                      (TVariable (TypeIndex KType 1) <| TVariable (TypeIndex KType 0) :| [])
                  )
                  "MkPair"
              )
              [ PVariable () (Label (TVariable (TypeIndex KType 1)) "fst")
              , PVariable () (Label (TVariable (TypeIndex KType 0)) "snd")
              ]
          )
          (CPlain () [] (ELiteral () (LBool True)) :| [])
          :| []
      )
  )

-- match(p : Pair(int32, bool)) { | MkPair(fst, snd) => true }
fixture13 :: Expression () ()
fixture13 =
  ( EMatch
      ()
      ()
      ( EAnnotation
          ()
          (TApplication () (TConstructor () "Pair") (TIntrinsic IInt32 <| TIntrinsic IBool :| []))
          (EVariable () (Label () "p"))
      )
      ( EClause
          ()
          (PConstructor () (Label () "MkPair") [PVariable () (Label () "fst"), PVariable () (Label () "snd")])
          (CPlain () [] (ELiteral () (LBool True)) :| [])
          :| []
      )
  )

fixture13Typed :: Expression () (Type TypeIndex Kind)
fixture13Typed =
  ( EMatch
      ()
      (TIntrinsic IBool)
      ( EAnnotation
          ()
          (TApplication () (TConstructor () "Pair") (TIntrinsic IInt32 <| TIntrinsic IBool :| []))
          (EVariable () (Label (TApplication KType (TConstructor (KArrow KType (KArrow KType KType)) "Pair") (TIntrinsic IInt32 :| [TIntrinsic IBool])) "p"))
      )
      ( EClause
          ()
          ( PConstructor
              ()
              ( Label
                  (TApplication KType (TConstructor (KArrow KType (KArrow KType KType)) "Pair") (TIntrinsic IInt32 <| TIntrinsic IBool :| []))
                  "MkPair"
              )
              [PVariable () (Label (TIntrinsic IInt32) "fst"), PVariable () (Label (TIntrinsic IBool) "snd")]
          )
          (CPlain () [] (ELiteral () (LBool True)) :| [])
          :| []
      )
  )

-- match(p : Pair(a, b)) { | MkPair(fst, snd) => true }
fixture14 :: Expression () ()
fixture14 =
  ( EMatch
      ()
      ()
      ( EAnnotation
          ()
          (TApplication () (TConstructor () "Pair") (TVariable (TypeParam () "a") <| TVariable (TypeParam () "b") :| []))
          (EVariable () (Label () "p"))
      )
      ( EClause
          ()
          (PConstructor () (Label () "MkPair") [PVariable () (Label () "fst"), PVariable () (Label () "snd")])
          (CPlain () [] (ELiteral () (LBool True)) :| [])
          :| []
      )
  )

fixture14Typed :: Expression () (Type TypeIndex Kind)
fixture14Typed =
  ( EMatch
      ()
      (TIntrinsic IBool)
      ( EAnnotation
          ()
          (TApplication () (TConstructor () "Pair") (TVariable (TypeParam () "a") <| TVariable (TypeParam () "b") :| []))
          (EVariable () (Label (TApplication KType (TConstructor (KArrow KType (KArrow KType KType)) "Pair") (TVariable (TypeIndex KType 1) :| [TVariable (TypeIndex KType 0)])) "p"))
      )
      ( EClause
          ()
          ( PConstructor
              ()
              ( Label
                  (TApplication KType (TConstructor (KArrow KType (KArrow KType KType)) "Pair") (TVariable (TypeIndex KType 1) <| TVariable (TypeIndex KType 0) :| []))
                  "MkPair"
              )
              [PVariable () (Label (TVariable (TypeIndex KType 1)) "fst"), PVariable () (Label (TVariable (TypeIndex KType 0)) "snd")]
          )
          (CPlain () [] (ELiteral () (LBool True)) :| [])
          :| []
      )
  )

-- match(p : Pair(a, a)) { | MkPair(fst, snd) => true }
fixture15 :: Expression () ()
fixture15 =
  ( EMatch
      ()
      ()
      ( EAnnotation
          ()
          (TApplication () (TConstructor () "Pair") (TVariable (TypeParam () "a") <| TVariable (TypeParam () "a") :| []))
          (EVariable () (Label () "p"))
      )
      ( EClause
          ()
          (PConstructor () (Label () "MkPair") [PVariable () (Label () "fst"), PVariable () (Label () "snd")])
          (CPlain () [] (ELiteral () (LBool True)) :| [])
          :| []
      )
  )

fixture15Typed :: Expression () (Type TypeIndex Kind)
fixture15Typed =
  ( EMatch
      ()
      (TIntrinsic IBool)
      ( EAnnotation
          ()
          (TApplication () (TConstructor () "Pair") (TVariable (TypeParam () "a") <| TVariable (TypeParam () "a") :| []))
          (EVariable () (Label (TApplication KType (TConstructor (KArrow KType (KArrow KType KType)) "Pair") (TVariable (TypeIndex KType 0) :| [TVariable (TypeIndex KType 0)])) "p"))
      )
      ( EClause
          ()
          ( PConstructor
              ()
              ( Label
                  (TApplication KType (TConstructor (KArrow KType (KArrow KType KType)) "Pair") (TVariable (TypeIndex KType 0) <| TVariable (TypeIndex KType 0) :| []))
                  "MkPair"
              )
              [PVariable () (Label (TVariable (TypeIndex KType 0)) "fst"), PVariable () (Label (TVariable (TypeIndex KType 0)) "snd")]
          )
          (CPlain () [] (ELiteral () (LBool True)) :| [])
          :| []
      )
  )

-- TODO
fixture16 :: Expression () ()
fixture16 =
  ( EMatch
      ()
      ()
      (EVariable () (Label () "x"))
      ( EClause
          ()
          (PConstructor () (Label () "Yes") [])
          (CPlain () [] (ELiteral () (LBool True)) :| [])
          <| EClause
            ()
            (PConstructor () (Label () "Foo") [])
            (CPlain () [] (ELiteral () (LBool False)) :| [])
            :| []
      )
  )

-- let f = fn(g, x) => g(x) in f
fixture17 :: Expression () ()
fixture17 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label () "f"))
        ( ELambda
            ()
            (PVariable () (Label () "g") :| [PVariable () (Label () "x")])
            ( EApplication
                ()
                ()
                (EVariable () (Label () "g"))
                (EVariable () (Label () "x") :| [])
            )
        )
        :| []
    )
    (EVariable () (Label () "f"))

fixture17Typed :: Expression () (Type TypeIndex Kind)
fixture17Typed =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label ((TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 1)) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 1)) "f"))
        ( ELambda
            ()
            ( PVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 1)) "g")
                :| [PVariable () (Label (TVariable (TypeIndex KType 0)) "x")]
            )
            ( EApplication
                ()
                (TVariable (TypeIndex KType 1))
                (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 1)) "g"))
                (EVariable () (Label (TVariable (TypeIndex KType 0)) "x") :| [])
            )
        )
        :| []
    )
    (EVariable () (Label ((TVariable (TypeIndex KType 3) `TArrow` TVariable (TypeIndex KType 2)) `TArrow` TVariable (TypeIndex KType 3) `TArrow` TVariable (TypeIndex KType 2)) "f"))

-- let f = fn(x : int32) => 1 in f
fixture18 :: Expression () ()
fixture18 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label () "f"))
        ( ELambda
            ()
            ( ( PAnnotation
                  ()
                  (TIntrinsic IInt32)
                  (PVariable () (Label () "x"))
              )
                :| []
            )
            (ELiteral () (LInt32 1))
        )
        :| []
    )
    (EVariable () (Label () "f"))

-- let f = fn(x : bool) => 1 in f
fixture19 :: Expression () ()
fixture19 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label () "f"))
        ( ELambda
            ()
            ( ( PAnnotation
                  ()
                  (TIntrinsic IBool)
                  (PVariable () (Label () "x"))
              )
                :| []
            )
            (ELiteral () (LInt32 1))
        )
        :| []
    )
    (EVariable () (Label () "f"))

-- let f = fn(x : bool) => x : bool in f
fixture21 :: Expression () ()
fixture21 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label () "f"))
        ( ELambda
            ()
            ( ( PAnnotation
                  ()
                  (TIntrinsic IBool)
                  (PVariable () (Label () "x"))
              )
                :| []
            )
            ( EAnnotation
                ()
                (TIntrinsic IBool)
                (EVariable () (Label () "x"))
            )
        )
        :| []
    )
    (EVariable () (Label () "f"))

-- let f = fn(x : bool) => x : int32 in f
fixture22 :: Expression () ()
fixture22 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label () "f"))
        ( ELambda
            ()
            ( ( PAnnotation
                  ()
                  (TIntrinsic IBool)
                  (PVariable () (Label () "x"))
              )
                :| []
            )
            ( EAnnotation
                ()
                (TIntrinsic IInt32)
                (EVariable () (Label () "x"))
            )
        )
        :| []
    )
    (EVariable () (Label () "f"))

-- let f = fn(x : a) => x : int32 in f
fixture23 :: Expression () ()
fixture23 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label () "f"))
        ( ELambda
            ()
            ( ( PAnnotation
                  ()
                  (TVariable (TypeParam () "a"))
                  (PVariable () (Label () "x"))
              )
                :| []
            )
            ( EAnnotation
                ()
                (TIntrinsic IInt32)
                (EVariable () (Label () "x"))
            )
        )
        :| []
    )
    (EVariable () (Label () "f"))

-- let
--   f =
--     fn(g : a -> b, x : c) =>
--       g(x)
--   in
--     f
