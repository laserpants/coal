{-# LANGUAGE OverloadedStrings #-}

-- module Noll.SystemFSpec (spec) where
module Noll.SystemFSpec where

import Control.Monad.Reader (runReader)
import Data.List.NonEmpty ((<|))
import Lang.Common.List1 (NonEmpty (..))
import Lang.Label (Label (..))
import Noll.Compiler.Transform.Fold
import Noll.Compiler.Transform.Type.AliasInsertion
import Noll.Language (
  BinaryOperator (..),
  Binding (..),
  Choice (..),
  Clause (..),
  Expression (..),
  IndexedType,
  Intrinsic (..),
  Kind (..),
  Parameter (..),
  Pattern (..),
  Primitive (..),
  Row (..),
  Scheme (..),
  Type (..),
  TypeIndex (..),
  With (..),
 )
import Noll.Module (Constant (..), Function (..), Module (..))
import Noll.SystemF.Constraint.Assumption (Assumption (..))
import Noll.SystemF.Constraint.Generation.Internal (InferenceRule (..))
import Noll.SystemFSpec.TestRunner
import Test.Hspec (Spec, describe, it)

import qualified Data.Set as Set
import qualified Lang.Common.Environment as Environment
import qualified Noll.CompilerExamples.Test02
import qualified Noll.Set.Test01
import qualified Noll.Set.Test02
import qualified Noll.Set.Test03
import qualified Noll.Set.Test04

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
        typedExpressionShouldMatch fixture2Typed fixture2
      it "" $
        hasNoErrors fixture2
      it "match(p) { | MkPair(fst, snd) => true }" $ do
        typedExpressionShouldMatch fixture12Typed fixture12
      it "" $
        hasNoErrors fixture12
      it "match(p : Pair(int32, bool)) { | MkPair(fst, snd) => true }" $ do
        typedExpressionShouldMatch fixture13Typed fixture13
      it "" $
        hasNoErrors fixture13
      it "match(p : Pair(a, b)) { | MkPair(fst, snd) => true }" $ do
        typedExpressionShouldMatch fixture14Typed fixture14
      it "" $
        hasNoErrors fixture14
      it "match(p : Pair(a, a)) { | MkPair(fst, snd) => true }" $ do
        typedExpressionShouldMatch fixture15Typed fixture15
      it "" $
        hasNoErrors fixture15
      it "" $ do
        typedExpressionShouldMatch fixture17Typed fixture17
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
    describe "" $ do
      it "" $ do
        typedFunctionShouldMatch
          [
            ( "not"
            , Forall
                mempty
                []
                (TIntrinsic IBool `TArrow` TIntrinsic IBool)
            )
          ,
            ( "less_than_or_equal_to"
            , Forall
                (Set.fromList [TypeIndex KType 0])
                []
                ( TVariable (TypeIndex KType 0)
                    `TArrow` TVariable (TypeIndex KType 0)
                    `TArrow` TIntrinsic IBool
                )
            )
          ]
          fixture29Typed
          fixture29
      it "" $ do
        typedExpressionShouldMatch fixture30Typed fixture30
      it "" $ do
        typedExpressionShouldMatch fixture31Typed fixture31
    describe "" $ do
      it "" $ do
        typedExpressionShouldMatch fixture37 fixture36
      it "" $ do
        typedExpressionShouldMatch fixture39 fixture38
      it "" $ do
        typedExpressionErrors2Includes (InferAnnotation () (TIntrinsic IInt32) (TIntrinsic IBool)) fixture40

-- typedExpression_ :: Function Expression () () -> Function Expression () (Type TypeIndex Kind)
typedExpression_ names e = testRunner runTypedExpressionTest names e

-- typedFunction_ :: Function Expression () () -> Function Expression () (Type TypeIndex Kind)
typedFunction_ names f = testRunner runTypedFunctionTest names f

typedFunction :: Function Expression () () -> Function Expression () (Type TypeIndex Kind)
typedFunction f = testResultExpression (testRunner runTypedFunctionTest mempty f)

-- typedFunctionShouldMatch :: Function Expression () (Type TypeIndex Kind) -> Function Expression () () -> Bool
typedFunctionShouldMatch names f0 f = testResultExpression (testRunner runTypedFunctionTest names f) == f0

typedExpressionErrors2Includes e f = e `elem` testResultErrors2 (testRunner runTypedExpressionTest mempty f)

typedExpression :: Expression () () -> Expression () (Type TypeIndex Kind)
typedExpression e = testResultExpression (testRunner runTypedExpressionTest mempty e)

typedExpressionShouldMatch :: Expression () (Type TypeIndex Kind) -> Expression () () -> Bool
typedExpressionShouldMatch e0 e = testResultExpression (testRunner runTypedExpressionTest mempty e) == e0

assumptions :: Expression () () -> [Assumption IndexedType]
assumptions e = testResultAssumptions (testRunner runTypedExpressionTest mempty e)

hasNoAssumptions :: Expression () () -> Bool
hasNoAssumptions e = null (testResultAssumptions (testRunner runTypedExpressionTest mempty e))

hasNoErrors :: Expression () () -> Bool
hasNoErrors e = null errs1 && null errs2
 where
  errs1 = testResultErrors1 result
  errs2 = testResultErrors2 result
  result = testRunner runTypedExpressionTest mempty e

numberOfErrors :: Expression () () -> Int
numberOfErrors e = length errs1 + length errs2
 where
  errs1 = testResultErrors1 result
  errs2 = testResultErrors2 result
  result = testRunner runTypedExpressionTest mempty e

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
  ELambda
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

-- -- TODO
-- -- let x = 1 in x(x)
-- fixture3 :: Expression () ()
-- fixture3 =
--   ELet
--     ()
--     ( BPattern
--         ()
--         (PVariable () (Label () "x"))
--         (ELiteral () (LInt32 1))
--         :| []
--     )
--     ( EApplication
--         ()
--         ()
--         (EVariable () (Label () "x"))
--         (EVariable () (Label () "x") :| [])
--     )
--
-- -- TODO
-- -- if 1 then 2 else 3
-- fixture4 :: Expression String ()
-- fixture4 =
--   EIf
--     "if"
--     ()
--     (ELiteral "b" (LInt32 1))
--     (ELiteral "c" (LInt32 2))
--     (ELiteral "d" (LInt32 3))

-- -- TODO
-- -- if true then 2 else false
-- fixture5 :: Expression String ()
-- fixture5 =
--   EIf
--     "if"
--     ()
--     (ELiteral "b" (LBool True))
--     (ELiteral "c" (LInt32 2))
--     (ELiteral "d" (LBool False))

-- -- TODO
-- -- if 1 then 2 else (if true then false else 2)
-- fixture6 :: Expression String ()
-- fixture6 =
--   EIf
--     "EIf-1"
--     ()
--     (ELiteral "ELiteral-1" (LInt32 1))
--     (ELiteral "ELiteral-2" (LInt32 2))
--     ( EIf
--         "EIf-2"
--         ()
--         (ELiteral "ELiteral-3" (LBool True))
--         (ELiteral "ELiteral-4" (LBool False))
--         (ELiteral "ELiteral-5" (LInt32 2))
--     )

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
  EMatch
    ()
    (TIntrinsic IBool)
    (EVariable () (Label (TConstructor KType "Answer") "x"))
    ( EClause
        ()
        (PConstructor () (Label (TConstructor KType "Answer") "Yes") [])
        (CPlain () [] (ELiteral () (LBool True)) :| [])
        :| []
    )

-- match(p) { | MkPair(fst, snd) => true }
fixture12 :: Expression () ()
fixture12 =
  EMatch
    ()
    ()
    (EVariable () (Label () "p"))
    ( EClause
        ()
        (PConstructor () (Label () "MkPair") [PVariable () (Label () "fst"), PVariable () (Label () "snd")])
        (CPlain () [] (ELiteral () (LBool True)) :| [])
        :| []
    )

fixture12Typed :: Expression () (Type TypeIndex Kind)
fixture12Typed =
  EMatch
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

-- match(p : Pair(int32, bool)) { | MkPair(fst, snd) => true }
fixture13 :: Expression () ()
fixture13 =
  EMatch
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

fixture13Typed :: Expression () (Type TypeIndex Kind)
fixture13Typed =
  EMatch
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

-- match(p : Pair(a, b)) { | MkPair(fst, snd) => true }
fixture14 :: Expression () ()
fixture14 =
  EMatch
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

fixture14Typed :: Expression () (Type TypeIndex Kind)
fixture14Typed =
  EMatch
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

-- match(p : Pair(a, a)) { | MkPair(fst, snd) => true }
fixture15 :: Expression () ()
fixture15 =
  EMatch
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

fixture15Typed :: Expression () (Type TypeIndex Kind)
fixture15Typed =
  EMatch
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

-- -- TODO
-- fixture16 :: Expression () ()
-- fixture16 =
--   ( EMatch
--       ()
--       ()
--       (EVariable () (Label () "x"))
--       ( EClause
--           ()
--           (PConstructor () (Label () "Yes") [])
--           (CPlain () [] (ELiteral () (LBool True)) :| [])
--           <| EClause
--             ()
--             (PConstructor () (Label () "Foo") [])
--             (CPlain () [] (ELiteral () (LBool False)) :| [])
--             :| []
--       )
--   )

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

-- -- let f = fn(x : int32) => 1 in f
-- fixture18 :: Expression () ()
-- fixture18 =
--   ELet
--     ()
--     ( BPattern
--         ()
--         (PVariable () (Label () "f"))
--         ( ELambda
--             ()
--             ( ( PAnnotation
--                   ()
--                   (TIntrinsic IInt32)
--                   (PVariable () (Label () "x"))
--               )
--                 :| []
--             )
--             (ELiteral () (LInt32 1))
--         )
--         :| []
--     )
--     (EVariable () (Label () "f"))

-- -- let f = fn(x : bool) => 1 in f
-- fixture19 :: Expression () ()
-- fixture19 =
--   ELet
--     ()
--     ( BPattern
--         ()
--         (PVariable () (Label () "f"))
--         ( ELambda
--             ()
--             ( ( PAnnotation
--                   ()
--                   (TIntrinsic IBool)
--                   (PVariable () (Label () "x"))
--               )
--                 :| []
--             )
--             (ELiteral () (LInt32 1))
--         )
--         :| []
--     )
--     (EVariable () (Label () "f"))

-- -- let f = fn(x : bool) => x : bool in f
-- fixture21 :: Expression () ()
-- fixture21 =
--   ELet
--     ()
--     ( BPattern
--         ()
--         (PVariable () (Label () "f"))
--         ( ELambda
--             ()
--             ( ( PAnnotation
--                   ()
--                   (TIntrinsic IBool)
--                   (PVariable () (Label () "x"))
--               )
--                 :| []
--             )
--             ( EAnnotation
--                 ()
--                 (TIntrinsic IBool)
--                 (EVariable () (Label () "x"))
--             )
--         )
--         :| []
--     )
--     (EVariable () (Label () "f"))

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

-- -- let f = fn(x : bool) => x : a in f
-- fixture24 :: Expression () ()
-- fixture24 =
--   ELet
--     ()
--     ( BPattern
--         ()
--         (PVariable () (Label () "f"))
--         ( ELambda
--             ()
--             ( ( PAnnotation
--                   ()
--                   (TIntrinsic IBool)
--                   (PVariable () (Label () "x"))
--               )
--                 :| []
--             )
--             ( EAnnotation
--                 ()
--                 (TVariable (Parameter () "a"))
--                 (EVariable () (Label () "x"))
--             )
--         )
--         :| []
--     )
--     (EVariable () (Label () "f"))

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

fixture29 :: Function Expression () ()
fixture29 =
  Function
    ()
    (With [] ())
    ( PAnnotation
        ()
        (TVariable (Parameter () "a"))
        (PVariable () (Label () "n"))
        :| []
    )
    ( EApplication
        ()
        ()
        (EBinaryOperator () () OReverseComposition)
        ( EVariable () (Label () "not")
            <| EApplication
              ()
              ()
              (EVariable () (Label () "less_than_or_equal_to"))
              (EVariable () (Label () "n") :| [])
              :| []
        )
    )

fixture29Typed :: Function Expression () (Type TypeIndex Kind)
fixture29Typed =
  Function
    ()
    (With [] (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool))
    ( PAnnotation
        ()
        (TVariable (Parameter () "a"))
        (PVariable () (Label (TVariable (TypeIndex KType 0)) "n"))
        :| []
    )
    ( EApplication
        ()
        (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
        ( EBinaryOperator
            ()
            (TArrow (TIntrinsic IBool `TArrow` TIntrinsic IBool) (TArrow (TArrow (TVariable (TypeIndex KType 0)) (TIntrinsic IBool)) (TArrow (TVariable (TypeIndex KType 0)) (TIntrinsic IBool))))
            OReverseComposition
        )
        ( EVariable () (Label (TIntrinsic IBool `TArrow` TIntrinsic IBool) "not")
            <| EApplication
              ()
              (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
              (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
              (EVariable () (Label (TVariable (TypeIndex KType 0)) "n") :| [])
              :| []
        )
    )

--
-- let
--   f =
--     fn(x) =>
--       x
--   in
--     f(1)
--
fixture30 :: Expression () ()
fixture30 =
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
        (EVariable () (Label () "f"))
        (ELiteral () (LInt32 1) :| [])
    )

fixture30Typed :: Expression () IndexedType
fixture30Typed =
  ELet
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
        (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f"))
        (ELiteral () (LInt32 1) :| [])
    )

-- let
--   f(x) = x
--   in
--     f(1)
--
fixture31 :: Expression () ()
fixture31 =
  ELet
    ()
    ( BFunction
        ()
        "f"
        (PVariable () (Label () "x") :| [])
        (EVariable () (Label () "x"))
        :| []
    )
    ( EApplication
        ()
        ()
        (EVariable () (Label () "f"))
        (ELiteral () (LInt32 1) :| [])
    )

fixture31Typed :: Expression () IndexedType
fixture31Typed =
  ELet
    ()
    ( BFunction
        ()
        "f"
        (PVariable () (Label (TVariable (TypeIndex KType 0)) "x") :| [])
        (EVariable () (Label (TVariable (TypeIndex KType 0)) "x"))
        :| []
    )
    ( EApplication
        ()
        (TIntrinsic IInt32)
        (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f"))
        (ELiteral () (LInt32 1) :| [])
    )

-- baz =
--  Noll.SystemFSpec.typedExpression_
--    []
--    --      ( "not"
--    --      , Forall
--    --          mempty
--    --          []
--    --          (TIntrinsic IBool `TArrow` TIntrinsic IBool)
--    --      )
--    --    ,
--    --      ( "less_than_or_equal_to"
--    --      , Forall
--    --          (Set.fromList [TypeIndex KType 0])
--    --          []
--    --          ( TVariable (TypeIndex KType 0)
--    --              `TArrow` TVariable (TypeIndex KType 0)
--    --              `TArrow` TIntrinsic IBool
--    --          )
--    --      )
--
--    Noll.SystemFSpec.fixture30

-- fn(m) => let y = m in let x = y(true) in x
fixture10 :: Expression () ()
fixture10 =
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

fixture36 :: Expression () ()
fixture36 =
  EAnnotation
    ()
    (TIntrinsic IBool)
    (EVariable () (Label () "a"))

fixture37 :: Expression () IndexedType
fixture37 =
  EAnnotation
    ()
    (TIntrinsic IBool)
    (EVariable () (Label (TIntrinsic IBool) "a"))

fixture38 :: Expression () ()
fixture38 =
  EAnnotation
    ()
    (TIntrinsic IBool)
    (ELiteral () (LBool True))

fixture39 :: Expression () IndexedType
fixture39 =
  EAnnotation
    ()
    (TIntrinsic IBool)
    (ELiteral () (LBool True))

fixture40 :: Expression () ()
fixture40 =
  EAnnotation
    ()
    (TIntrinsic IBool)
    (ELiteral () (LInt32 1))

story = do
  it "" $
    runReader (insertAliases Noll.Set.Test01.prog1_01) testEnvironment2 == Noll.Set.Test02.prog1_02
  it "" $
    runFoldExpansion "fold" 1 (compileFolds Noll.Set.Test02.prog1_02) == Noll.Set.Test03.prog1_03
  it "" $
    testResultExpression (Noll.CompilerExamples.Test02.baz3 Noll.Set.Test03.moduleUtils) == Noll.Set.Test04.moduleUtils
  it "" $
    testResultExpression (Noll.CompilerExamples.Test02.baz3 Noll.Set.Test03.moduleOrdered) == Noll.Set.Test04.moduleOrdered
  it "" $
    testResultExpression (Noll.CompilerExamples.Test02.baz3 Noll.Set.Test03.moduleBinarySearch) == Noll.Set.Test04.moduleBinarySearch
  it "" $
    testResultExpression (Noll.CompilerExamples.Test02.baz3 Noll.Set.Test03.moduleMain) == Noll.Set.Test04.moduleMain

testEnvironment2 :: AliasEnvironment
testEnvironment2 =
  Environment.fromList
    [
      ( "Predicate"
      ,
        ( ["a"]
        , TVariable (Parameter () "a") `TArrow` TIntrinsic IBool
        )
      )
    ,
      ( "Range"
      ,
        ( ["a"]
        , ( TIntrinsic
              ( IRecord
                  ( TRow
                      ( RExtend
                          "max"
                          (TVariable (Parameter () "a"))
                          ( RExtend
                              "min"
                              (TVariable (Parameter () "a"))
                              RNil
                          )
                      )
                  )
              )
          )
        )
      )
    ]
