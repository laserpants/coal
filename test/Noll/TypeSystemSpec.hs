{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Noll.TypeSystemSpec (spec) where

import Control.Monad.State (evalState)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.Set as Set
import Debug.Trace
import Noll.Label (Label (..))
import Noll.Language (Binding (..), Choice (..), Clause (..), Constructor (..), Expression (..), Intrinsic (..), Kind (..), KindIndex (..), Pattern (..), Primitive (..), Scheme (..), Type (..), TypeId (..), TypeIndex (..), freshIdIn)
import Noll.Library.Environment (Environment (..))
import qualified Noll.Library.Environment as Environment
import Noll.Library.Supply (supply)
import Noll.TypeSystem.ConstraintSolver (SolverError (..), evalSolver, solveKinds, solveTypes)
import Noll.TypeSystem.KindConstraint (KindConstraint (..), KindConstraintMetadata (..))
import Noll.TypeSystem.KindConstraint.Collect (collectKindConstraints, runCollectKindConstraints)
import Noll.TypeSystem.KindSubstitution (KindSubstitution (..), applyKindSub)
import Noll.TypeSystem.TypeConstraint (Descriptor (..), TypeConstraint (..))
import Noll.TypeSystem.TypeConstraint.Collect (TypeConstraintsContext (..), collectTypeConstraints, evalCollectTypeConstraints)
import Noll.TypeSystem.TypeSubstitution (TypeSubstitutable (..), TypeSubstitution, normalizeTypeIndexes)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem" $ do
    it "" $
      validateResult fixture1 == fixture1Typed
    it "" $
      hasNoErrors fixture1
    it "" $
      validateResult fixture2 == fixture2Typed
    it "" $
      hasNoErrors fixture2
    it "" $
      validateResult fixture7 == fixture7Typed
    it "" $
      hasNoErrors fixture7
    it "" $
      validateResult fixture9 == fixture9Typed
    it "" $
      hasNoErrors fixture9
    it "" $
      validateResult fixture10 == fixture10Typed
    it "" $
      hasNoErrors fixture10
    it "" $
      validateResult fixture11 == fixture11Typed
    it "" $
      hasNoErrors fixture11
    it "" $
      validateResult fixture12 == fixture12Typed
    it "" $
      hasNoErrors fixture12
    it "" $
      validateResult fixture13 == fixture13Typed
    it "" $
      hasNoErrors fixture13
    it "" $
      validateResult fixture14 == fixture14Typed
    it "" $
      hasNoErrors fixture14
    it "" $
      validateResult fixture15 == fixture15Typed
    it "" $
      hasNoErrors fixture15
    it "" $
      validateResult fixture17 == fixture17Typed
    it "" $
      hasNoErrors fixture17
    it "" $
      validateResult fixture18 == fixture18Typed
    it "" $
      hasNoErrors fixture18
    it "" $
      typeErrorsInclude fixture4 (SolverError (RuleIfCondition "if"))
    it "" $
      typeErrorsInclude
        fixture5
        (SolverError (RuleIfBranches "if" (TIntrinsic IInt32) (TIntrinsic IBool)))
    it "" $
      typeErrorsIncludeAll
        fixture6
        [ SolverError (RuleIfBranches "if-2" (TIntrinsic IBool) (TIntrinsic IInt32))
        , SolverError (RuleIfCondition "if-1")
        ]
    it "" $
      typeErrorsInclude
        fixture16
        (SolverError (RuleMatchClausePatterns ()))
    it "" $
      typeErrorsInclude
        fixture19
        (SolverError (RuleApplication "b" (typeVariable 2) (TIntrinsic IBool `TArrow` TIntrinsic IInt32 `TArrow` typeVariable 1)))

typeErrorsIncludeAll :: (Show a, Eq a) => Expression a () -> [SolverError (Descriptor (Kind KindIndex) a)] -> Bool
typeErrorsIncludeAll e = all (typeErrorsInclude e)

typeErrorsInclude :: (Show a, Eq a) => Expression a () -> SolverError (Descriptor (Kind KindIndex) a) -> Bool
typeErrorsInclude e err = err `elem` errs
 where
  (_, errs, _) = testInferTypes e

kindErrorsInclude :: (Show a, Eq a) => Expression a () -> SolverError KindConstraintMetadata -> Bool
kindErrorsInclude e err = err `elem` errs
 where
  (_, _, errs) = testInferTypes e

hasNoErrors :: (Show a, Eq a) => Expression a () -> Bool
hasNoErrors e = hasNoTypeErrors e && hasNoKindErrors e

hasNoTypeErrors :: (Show a, Eq a) => Expression a () -> Bool
hasNoTypeErrors e = let (_, errs, _) = testInferTypes e in null errs

hasNoKindErrors :: (Show a, Eq a) => Expression a () -> Bool
hasNoKindErrors e = let (_, _, errs) = testInferTypes e in null errs

validateResult :: (Show a, Eq a) => Expression a () -> Expression a (Type TypeIndex (Kind KindIndex))
validateResult e = let (a, _, _) = testInferTypes e in a

type Result a =
  ( Expression a (Type TypeIndex (Kind KindIndex))
  , [SolverError (Descriptor (Kind KindIndex) a)]
  , [SolverError KindConstraintMetadata]
  )

testInferTypes :: forall a. (Show a, Eq a) => Expression a () -> Result a
testInferTypes e =
  let
    e0 :: Expression a Int
    e0 = evalState (traverse (const supply) e) (0 :: Int)

    e1 :: Expression a (Type TypeIndex (Kind KindIndex))
    e1 = fmap typeVariable e0

    constructorEnv :: Environment (Constructor TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex)))
    constructorEnv =
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
              ( Forall
                  mempty
                  []
                  ( TVariable (TypeIndex KType 0)
                      `TArrow` TApplication
                        KType
                        (TConstructor (KArrow KType KType) "Id")
                        (TVariable (TypeIndex KType 0) :| [])
                  )
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

    typeConstraints :: [TypeConstraint (Descriptor (Kind KindIndex) a) TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))]
    typeConstraints =
      evalCollectTypeConstraints
        (freshIdIn e1)
        (TypeConstraintsContext mempty constructorEnv)
        (collectTypeConstraints e1)

    res1 :: (TypeSubstitution, [SolverError (Descriptor (Kind KindIndex) a)])
    res1 = evalSolver (freshIdIn typeConstraints) (solveTypes typeConstraints)

    (typeSub, errs1) = res1

    e2 :: Expression a (Type TypeIndex (Kind KindIndex))
    e2 = apply typeSub e1

    typeConstructorEnv :: Environment (Kind KindIndex)
    typeConstructorEnv =
      Environment.fromList
        [ ("Answer", KType)
        , ("Pair1", KArrow KType KType) -- Homogeneous pair type
        , ("Pair", KArrow KType (KArrow KType KType))
        , ("IntPair", KType)
        ]

    kindConstraints :: [KindConstraint KindConstraintMetadata (Kind KindIndex)]
    kindConstraints = runCollectKindConstraints typeConstructorEnv (collectKindConstraints e2)

    res2 :: (KindSubstitution, [SolverError KindConstraintMetadata])
    res2 = evalSolver 0 (solveKinds kindConstraints)

    (kindSub, errs2) = res2

    e3 :: Expression a (Type TypeIndex (Kind KindIndex))
    e3 = applyKindSub kindSub e2
   in
    -- traceShow e1 $
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

fixture1Typed :: Expression () (Type TypeIndex (Kind KindIndex))
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

fixture2Typed :: Expression () (Type TypeIndex (Kind KindIndex))
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

fixture7Typed :: Expression () (Type TypeIndex (Kind KindIndex))
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

-- TODO
fixture8 :: Expression () ()
fixture8 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label () "f"))
        ( ELambda
            ()
            (PVariable () (Label () "x") :| [])
            ( ELambda
                ()
                (PVariable () (Label () "y") :| [])
                ( EMatch
                    ()
                    ()
                    ( EApplication
                        ()
                        ()
                        (EVariable () (Label () "equals"))
                        (EVariable () (Label () "x") <| EVariable () (Label () "y") :| [])
                    )
                    ( EClause
                        ()
                        (PConstructor () (Label () "Yes") [])
                        (CPlain () [] (ELiteral () (LBool True)) :| [])
                        <| EClause
                          ()
                          (PConstructor () (Label () "No") [])
                          (CPlain () [] (ELiteral () (LBool False)) :| [])
                          :| []
                    )
                )
            )
        )
        :| []
    )
    (EVariable () (Label () "f"))

-- if true then 2 else 3
fixture9 :: Expression String ()
fixture9 =
  EIf
    "if"
    ()
    (ELiteral "a" (LBool True))
    (ELiteral "b" (LInt32 2))
    (ELiteral "c" (LInt32 3))

fixture9Typed :: Expression String (Type TypeIndex (Kind KindIndex))
fixture9Typed =
  EIf
    "if"
    (TIntrinsic IInt32)
    (ELiteral "a" (LBool True))
    (ELiteral "b" (LInt32 2))
    (ELiteral "c" (LInt32 3))

fixture10 :: Expression () ()
fixture10 =
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
            (PConstructor () (Label () "No") [])
            (CPlain () [] (ELiteral () (LBool False)) :| [])
            :| []
      )
  )

fixture10Typed :: Expression () (Type TypeIndex (Kind KindIndex))
fixture10Typed =
  ( EMatch
      ()
      (TIntrinsic IBool)
      (EVariable () (Label (TConstructor KType "Answer") "x"))
      ( EClause
          ()
          (PConstructor () (Label (TConstructor KType "Answer") "Yes") [])
          (CPlain () [] (ELiteral () (LBool True)) :| [])
          <| EClause
            ()
            (PConstructor () (Label (TConstructor KType "Answer") "No") [])
            (CPlain () [] (ELiteral () (LBool False)) :| [])
            :| []
      )
  )

fixture11 :: Expression () ()
fixture11 =
  ( EMatch
      ()
      ()
      (EVariable () (Label () "p"))
      ( EClause
          ()
          (PConstructor () (Label () "MkPair1") [PVariable () (Label () "fst"), PVariable () (Label () "snd")])
          (CPlain () [] (ELiteral () (LBool True)) :| [])
          :| []
      )
  )

fixture11Typed :: Expression () (Type TypeIndex (Kind KindIndex))
fixture11Typed =
  ( EMatch
      ()
      (TIntrinsic IBool)
      (EVariable () (Label (TApplication KType (TConstructor (KArrow KType KType) "Pair1") (TVariable (TypeIndex KType 0) :| [])) "p"))
      ( EClause
          ()
          ( PConstructor
              ()
              (Label (TApplication KType (TConstructor (KArrow KType KType) "Pair1") (TVariable (TypeIndex KType 0) :| [])) "MkPair1")
              [ PVariable () (Label (TVariable (TypeIndex KType 0)) "fst")
              , PVariable () (Label (TVariable (TypeIndex KType 0)) "snd")
              ]
          )
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

fixture12Typed :: Expression () (Type TypeIndex (Kind KindIndex))
fixture12Typed =
  ( EMatch
      ()
      (TIntrinsic IBool)
      (EVariable () (Label (TApplication KType (TConstructor (KArrow KType (KArrow KType KType)) "Pair") (TVariable (TypeIndex KType 1) :| [TVariable (TypeIndex KType 0)])) "p"))
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

-- match(p : Pair(int32, bool)) { | MkPair(fst, snd) => true }
fixture13 :: Expression () ()
fixture13 =
  ( EMatch
      ()
      ()
      ( EAnnotation
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

fixture13Typed :: Expression () (Type TypeIndex (Kind KindIndex))
fixture13Typed =
  ( EMatch
      ()
      (TIntrinsic IBool)
      ( EAnnotation
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
          (TApplication () (TConstructor () "Pair") (TVariable (TypeId () "a") <| TVariable (TypeId () "b") :| []))
          (EVariable () (Label () "p"))
      )
      ( EClause
          ()
          (PConstructor () (Label () "MkPair") [PVariable () (Label () "fst"), PVariable () (Label () "snd")])
          (CPlain () [] (ELiteral () (LBool True)) :| [])
          :| []
      )
  )

fixture14Typed :: Expression () (Type TypeIndex (Kind KindIndex))
fixture14Typed =
  ( EMatch
      ()
      (TIntrinsic IBool)
      ( EAnnotation
          (TApplication () (TConstructor () "Pair") (TVariable (TypeId () "a") <| TVariable (TypeId () "b") :| []))
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
          (TApplication () (TConstructor () "Pair") (TVariable (TypeId () "a") <| TVariable (TypeId () "a") :| []))
          (EVariable () (Label () "p"))
      )
      ( EClause
          ()
          (PConstructor () (Label () "MkPair") [PVariable () (Label () "fst"), PVariable () (Label () "snd")])
          (CPlain () [] (ELiteral () (LBool True)) :| [])
          :| []
      )
  )

fixture15Typed :: Expression () (Type TypeIndex (Kind KindIndex))
fixture15Typed =
  ( EMatch
      ()
      (TIntrinsic IBool)
      ( EAnnotation
          (TApplication () (TConstructor () "Pair") (TVariable (TypeId () "a") <| TVariable (TypeId () "a") :| []))
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

-- match(p) { | MkIntPair(fst, snd) => fst }
fixture17 :: Expression () ()
fixture17 =
  ( EMatch
      ()
      ()
      (EVariable () (Label () "p"))
      ( EClause
          ()
          (PConstructor () (Label () "MkIntPair") [PVariable () (Label () "fst"), PVariable () (Label () "snd")])
          (CPlain () [] (EVariable () (Label () "fst")) :| [])
          :| []
      )
  )

fixture17Typed :: Expression () (Type TypeIndex (Kind KindIndex))
fixture17Typed =
  ( EMatch
      ()
      (TIntrinsic IInt32)
      (EVariable () (Label (TConstructor KType "IntPair") "p"))
      ( EClause
          ()
          (PConstructor () (Label (TConstructor KType "IntPair") "MkIntPair") [PVariable () (Label (TIntrinsic IInt32) "fst"), PVariable () (Label (TIntrinsic IInt32) "snd")])
          (CPlain () [] (EVariable () (Label (TIntrinsic IInt32) "fst")) :| [])
          :| []
      )
  )

-- match(MkIntPair(1, 2)) { | MkIntPair(fst, snd) => fst }
fixture18 :: Expression () ()
fixture18 =
  ( EMatch
      ()
      ()
      ( EApplication
          ()
          ()
          (EConstructor () (Label () "MkIntPair"))
          (ELiteral () (LInt32 1) <| ELiteral () (LInt32 2) :| [])
      )
      ( EClause
          ()
          (PConstructor () (Label () "MkIntPair") [PVariable () (Label () "fst"), PVariable () (Label () "snd")])
          (CPlain () [] (EVariable () (Label () "fst")) :| [])
          :| []
      )
  )

fixture18Typed :: Expression () (Type TypeIndex (Kind KindIndex))
fixture18Typed =
  ( EMatch
      ()
      (TIntrinsic IInt32)
      ( EApplication
          ()
          (TConstructor KType "IntPair")
          (EConstructor () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TConstructor KType "IntPair") "MkIntPair"))
          (ELiteral () (LInt32 1) <| ELiteral () (LInt32 2) :| [])
      )
      ( EClause
          ()
          (PConstructor () (Label (TConstructor KType "IntPair") "MkIntPair") [PVariable () (Label (TIntrinsic IInt32) "fst"), PVariable () (Label (TIntrinsic IInt32) "snd")])
          (CPlain () [] (EVariable () (Label (TIntrinsic IInt32) "fst")) :| [])
          :| []
      )
  )

-- match(MkIntPair(false, 2)) { | MkIntPair(fst, snd) => fst }
fixture19 :: Expression String ()
fixture19 =
  ( EMatch
      "a"
      ()
      ( EApplication
          "b"
          ()
          (EConstructor "c" (Label () "MkIntPair"))
          (ELiteral "d" (LBool False) <| ELiteral "e" (LInt32 2) :| [])
      )
      ( EClause
          "f"
          (PConstructor "g" (Label () "MkIntPair") [PVariable "h" (Label () "fst"), PVariable "i" (Label () "snd")])
          (CPlain "j" [] (EVariable "k" (Label () "fst")) :| [])
          :| []
      )
  )
