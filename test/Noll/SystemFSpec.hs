{-# LANGUAGE OverloadedStrings #-}

module Noll.SystemFSpec where

import Control.Monad.State (evalState, gets)
import Control.Monad.Writer (execWriter)
import Data.Either.Extra (lefts, rights)
import Data.List.NonEmpty ((<|))
import Debug.Trace
import Noll.Common.Environment (Environment)
import Noll.Common.List1 (NonEmpty (..))
import Noll.Common.Supply (supply)
import Noll.Compiler (
  Compiler (..),
  CompilerEnvironment (..),
  CompilerState (..),
  evalCompiler,
  generateConstraintsC,
  getConstraintsGenerationErrorsC,
  getSolverRuleViolationsC,
  runCompiler,
  solveConstraintsC,
 )
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
  Parameter (..),
  freshIdIn,
  indexed,
 )
import Noll.SystemF.Constraint.Assumption (Assumption (..))
import Noll.SystemF.Constraint.Generation
import Noll.SystemF.Constraint.Generation.Internal (InferenceRule (..))
import Noll.SystemF.Constraint.Generation.TypeAnnotation (
  TypeAnnotationError (..),
  checkTypeAnnotationParameters,
 )
import Noll.SystemF.Constraint.Solver (solveConstraints)
import Noll.SystemF.Substitution (apply, normalizeTypeIndexes)
import Noll.SystemFSpec.TestRunner
import Test.Hspec (Spec, describe, hspec, it)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Common.Environment as Environment
import qualified Noll.SystemFFixtures.Expression

spec :: Spec
spec =
  describe "Noll.SystemF" $ do
    describe "match x { | Yes => true }" $ do
      it "" $ do
        typedExpressionShouldMatch fixture7Typed fixture7
      it "" $
        hasNoErrors fixture7
      it "" $
        assumptions fixture7 == [Assumption "x" (TConstructor KType "Answer")]
    describe "fn(m) => let y = m in let x = y(true) in x" $ do
      it "" $
        typedExpressionShouldMatch fixture1Typed fixture1
      it "" $
        hasNoErrors fixture1
      it "" $
        hasNoAssumptions fixture1
    describe "match x { | Yes => y }" $ do
      it "" $
        assumptions fixture28 == [Assumption "x" (TConstructor KType "Answer"), Assumption "y" (TVariable (TypeIndex KType 3))]

    it "" $
      hasNoErrors fixture1
    it "let f = fn(x) => x in (f(f))(f(1))" $ do
      typedExpression fixture2 == fixture2Typed
    it "" $
      hasNoErrors fixture2
    it "match(p) { | MkPair(fst, snd) => true }" $ do
      typedExpression fixture12 == fixture12Typed
    it "" $
      hasNoErrors fixture12
    it "match(p : Pair(int32, bool)) { | MkPair(fst, snd) => true }" $ do
      typedExpression fixture13 == fixture13Typed
    it "" $
      hasNoErrors fixture13
    it "match(p : Pair(a, b)) { | MkPair(fst, snd) => true }" $ do
      typedExpression fixture14 == fixture14Typed
    it "" $
      hasNoErrors fixture14
    it "match(p : Pair(a, a)) { | MkPair(fst, snd) => true }" $ do
      typedExpression fixture15 == fixture15Typed
    it "" $
      hasNoErrors fixture15
    it "" $ do
      typedExpression fixture17 == fixture17Typed
    it "" $
      hasNoErrors fixture17
    it "" $
      numberOfErrors fixture22 == 1
    it "" $
      numberOfErrors fixture23 == 1
    it "" $
      numberOfErrors fixture25 == 1
    it "" $
      numberOfErrors fixture26 == 1
    it "" $
      assumptions fixture27 == []

typedExpression :: Expression () () -> Expression () (Type TypeIndex Kind)
typedExpression e = testResultExpression (testRunner mempty e)

typedExpressionShouldMatch :: Expression () (Type TypeIndex Kind) -> Expression () () -> Bool
typedExpressionShouldMatch e0 e = testResultExpression (testRunner mempty e) == e0

assumptions :: Expression () () -> [Assumption IndexedType]
assumptions e = testResultAssumptions (testRunner mempty e)

hasNoAssumptions :: Expression () () -> Bool
hasNoAssumptions e = null (testResultAssumptions (testRunner mempty e))

hasNoErrors :: Expression () () -> Bool
hasNoErrors e = null errs1 && null errs2
 where
  errs1 = testResultErrors1 result
  errs2 = testResultErrors2 result
  result = testRunner mempty e

numberOfErrors :: Expression () () -> Int
numberOfErrors e = length errs1 + length errs2
 where
  errs1 = testResultErrors1 result
  errs2 = testResultErrors2 result
  result = testRunner mempty e

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
          (TApplication () (TConstructor () "Pair") (TVariable (Parameter () "a") <| TVariable (Parameter () "b") :| []))
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
          (TApplication () (TConstructor () "Pair") (TVariable (Parameter () "a") <| TVariable (Parameter () "b") :| []))
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
          (TApplication () (TConstructor () "Pair") (TVariable (Parameter () "a") <| TVariable (Parameter () "a") :| []))
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
          (TApplication () (TConstructor () "Pair") (TVariable (Parameter () "a") <| TVariable (Parameter () "a") :| []))
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
                  (TVariable (Parameter () "a"))
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

-- let f = fn(x : bool) => x : a in f
fixture24 :: Expression () ()
fixture24 =
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
                (TVariable (Parameter () "a"))
                (EVariable () (Label () "x"))
            )
        )
        :| []
    )
    (EVariable () (Label () "f"))

-- let f = fn(x : b) => x : a in f
fixture25 :: Expression () ()
fixture25 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label () "f"))
        ( ELambda
            ()
            ( ( PAnnotation
                  ()
                  (TVariable (Parameter () "b"))
                  (PVariable () (Label () "x"))
              )
                :| []
            )
            ( EAnnotation
                ()
                (TVariable (Parameter () "a"))
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
fixture26 :: Expression () ()
fixture26 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label () "f"))
        ( ELambda
            ()
            ( ( PAnnotation
                  ()
                  (TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "b"))
                  (PVariable () (Label () "g"))
              )
                <| ( PAnnotation
                      ()
                      (TVariable (Parameter () "c"))
                      (PVariable () (Label () "x"))
                   )
                  :| []
            )
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

-- fn(g, x) => g(x)
fixture27 :: Expression () ()
fixture27 =
  ELambda
    ()
    ( PVariable () (Label () "g")
        <| PVariable () (Label () "x")
          :| []
    )
    ( EApplication
        ()
        ()
        (EVariable () (Label () "g"))
        (EVariable () (Label () "x") :| [])
    )

-- match x { | Yes => y }
fixture28 :: Expression () ()
fixture28 =
  ( EMatch
      ()
      ()
      (EVariable () (Label () "x"))
      ( EClause
          ()
          (PConstructor () (Label () "Yes") [])
          (CPlain () [] (EVariable () (Label () "y")) :| [])
          :| []
      )
  )
