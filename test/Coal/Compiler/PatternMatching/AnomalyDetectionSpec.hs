{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.PatternMatching.AnomalyDetectionSpec (patternAnomaliesSpec) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Common.Label (Label (..))
import Coal.Compiler.Build
import Coal.Compiler.Build.NameEntry
import Coal.Compiler.Environment
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.PatternMatching.AnomalyDetection
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language
import Coal.Language.Module.Path (Path (..))
import Control.Monad.Identity (runIdentity)
import Control.Monad.State (put)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Test.Hspec

example1 :: [Pat]
example1 =
  [ Con "Cons" [Any, Con "Cons" [Any, Any]]
  , Con "Nil" mempty
  , Con "Cons" [Any, Any]
  ]

example2 :: [Pat]
example2 =
  [ Con "Cons" [Any, Con "Cons" [Any, Any]]
  , Con "Nil" mempty
  ]

example3 :: [Pat]
example3 =
  [ Con "Cons" [Any, Con "Cons" [Any, Any]]
  , Con "Cons" [Any, Any]
  ]

example4 :: [Pat]
example4 =
  [ Con "Cons" [Any, Con "Cons" [Any, Any]]
  , Con "Cons" [Any, Any]
  , Any
  ]

example5 :: [Pat]
example5 =
  [ Any
  ]

example12 :: [Pat]
example12 =
  [ Lit (LBool False)
  , Lit (LBool True)
  ]

example13 :: [Pat]
example13 =
  [ Con "Fez" [Lit (LBool False), Con "A" mempty]
  , Con "Fez" [Lit (LBool True), Con "B" mempty]
  ]

example14 :: [Pat]
example14 =
  [ Con "Fez" [Lit (LBool False), Con "A" mempty]
  , Con "Fez" [Lit (LBool True), Any]
  ]

example15 :: [Pat]
example15 =
  [ Con "Fez" [Lit (LBool False), Any]
  , Con "Fez" [Lit (LBool True), Con "A" mempty]
  ]

example16 :: [Pat]
example16 =
  [ Con "Fez" [Con "A" mempty, Con "A" mempty]
  , Con "Fez" [Con "B" mempty, Any]
  ]

example17 :: [Pat]
example17 =
  [ Con "Fez" [Con "A" mempty, Con "A" mempty]
  , Con "Fez" [Con "B" mempty, Con "B" mempty]
  ]

example18 :: [Pat]
example18 =
  [ Con "Fez" [Con "A" mempty, Any]
  , Con "Fez" [Con "B" mempty, Con "A" mempty]
  ]

example19 :: [Pat]
example19 =
  [ Con "Fez" [Con "A" mempty, Lit (LBool False)]
  , Con "Fez" [Con "B" mempty, Lit (LBool True)]
  ]

example20 :: [Pat]
example20 =
  [ Con "Fez" [Lit (LBool False), Con "A" mempty]
  , Con "Fez" [Lit (LBool False), Con "B" mempty]
  ]

testEnv :: Environment (DataConstructorEntry Metadata)
testEnv =
  Environment.fromList
    [
      ( "Cons"
      , DataConstructorEntry
          mempty
          "Cons"
          ( DataConstructor
              "Cons"
              2
              (Forall mempty mempty (applyTypeArgs KType (TConstructor (KArrow KType KType) "List") (TVariable (TypeIndex KType 0) :| mempty)))
          )
          (Set.fromList ["Cons", "Nil"])
      )
    ,
      ( "Nil"
      , DataConstructorEntry
          mempty
          "Nil"
          ( DataConstructor
              "Nil"
              0
              (Forall mempty mempty (applyTypeArgs KType (TConstructor (KArrow KType KType) "List") (TVariable (TypeIndex KType 0) :| mempty)))
          )
          (Set.fromList ["Cons", "Nil"])
      )
    ,
      ( "A"
      , DataConstructorEntry
          mempty
          "A"
          ( DataConstructor
              "A"
              0
              (Forall mempty mempty (TConstructor KType "X"))
          )
          (Set.fromList ["A", "B"])
      )
    ,
      ( "B"
      , DataConstructorEntry
          mempty
          "B"
          ( DataConstructor
              "B"
              0
              (Forall mempty mempty (TConstructor KType "X"))
          )
          (Set.fromList ["A", "B"])
      )
    ,
      ( "Fez"
      , DataConstructorEntry
          mempty
          "Fez"
          ( DataConstructor
              "Fez"
              2
              (Forall mempty mempty (TIntrinsic IBool `TArrow` TConstructor KType "X" `TArrow` TConstructor KType "Fez"))
          )
          (Set.fromList ["Fez"])
      )
    ]

runTest :: [Pat] -> Bool
runTest px =
  case runIdentity (evalCompilerT emptyCompilerEnvironment (setupEnv >> exhaustive px)) of
    Right r -> r
    Left err -> error ("Unexpected compiler failure: " ++ show err)
 where
  setupEnv = do
    put
      initialCompilerState
        { compilerCurrentPath = Path ["Test"]
        , compilerModules =
            Environment.fromList
              [
                ( "Test"
                , emptyBuild
                    { buildDataConstructors = testEnv
                    }
                )
              ]
        }

example6 :: [Pattern () () ()]
example6 =
  [ PConstructor () (Label () "Cons") [PVariable () (Label () "x"), PConstructor () (Label () "Cons") [PVariable () (Label () "y"), PVariable () (Label () "ys")]]
  , PConstructor () (Label () "Nil") mempty
  , PConstructor () (Label () "Cons") [PAny () (), PAny () ()]
  ]

example7 :: [Pattern () () ()]
example7 =
  [ PConstructor () (Label () "Cons") [PVariable () (Label () "x"), PConstructor () (Label () "Cons") [PVariable () (Label () "y"), PVariable () (Label () "ys")]]
  , PConstructor () (Label () "Nil") mempty
  ]

example8 :: [Pattern () () ()]
example8 =
  [ PConstructor () (Label () "Cons") [PVariable () (Label () "x"), PConstructor () (Label () "Cons") [PVariable () (Label () "y"), PVariable () (Label () "ys")]]
  , PConstructor () (Label () "Cons") [PAny () (), PAny () ()]
  ]

example9 :: [Pattern () () ()]
example9 =
  [ PConstructor () (Label () "Cons") [PVariable () (Label () "x"), PConstructor () (Label () "Cons") [PVariable () (Label () "y"), PVariable () (Label () "ys")]]
  , POr
      ()
      ()
      (PConstructor () (Label () "Nil") mempty)
      (PConstructor () (Label () "Cons") [PAny () (), PAny () ()])
  ]

example10 :: [Pattern () () ()]
example10 =
  [ PConstructor () (Label () "Cons") [PVariable () (Label () "x"), PConstructor () (Label () "Cons") [PVariable () (Label () "y"), PVariable () (Label () "ys")]]
  , PConstructor () (Label () "Cons") [PAny () (), PAny () ()]
  , PAny () ()
  ]

example11 :: [Pattern () () ()]
example11 =
  [ PAny () ()
  ]

runTest2 :: [Pattern a () t] -> Bool
runTest2 px =
  case runIdentity (evalCompilerT emptyCompilerEnvironment (setupEnv >> exhaustive (translatePattern <$> px))) of
    Right r -> r
    Left err -> error ("Unexpected compiler failure: " ++ show err)
 where
  setupEnv = do
    put
      initialCompilerState
        { compilerCurrentPath = Path ["Test"]
        , compilerModules =
            Environment.fromList
              [
                ( "Test"
                , emptyBuild
                    { buildDataConstructors = testEnv
                    }
                )
              ]
        }

patternAnomaliesSpec :: Spec
patternAnomaliesSpec =
  describe "PatternAnomalies" $ do
    describe "list patterns" $ do
      it "Cons(nested Cons) + Nil + Cons covers all lists" (runTest example1)
      it "detects missing Cons(Any, Any) case" (not $ runTest example2)
      it "detects missing Nil case" (not $ runTest example3)
      it "wildcard makes the match exhaustive" (runTest example4)
      it "a single wildcard covers everything" (runTest example5)

    describe "translated patterns" $ do
      it "Cons(nested Cons) + Nil + Cons covers all lists" (runTest2 example6)
      it "detects missing Cons(Any, Any) case" (not $ runTest2 example7)
      it "detects missing Nil case" (not $ runTest2 example8)
      it "or-pattern covering Nil + Cons is exhaustive" (runTest2 example9)
      it "wildcard makes the match exhaustive" (runTest2 example10)
      it "a single wildcard covers everything" (runTest2 example11)

    describe "boolean literal patterns" $ do
      it "False + True covers all booleans" (runTest example12)

    describe "nested constructor and literal patterns" $ do
      it "detects missing Fez(True, A) case" (not $ runTest example13)
      it "detects missing Fez(False, B) case" (not $ runTest example14)
      it "detects missing Fez(True, A) case" (not $ runTest example15)
      it "detects missing Fez(A, A) case" (not $ runTest example16)
      it "detects missing Fez(B, A) case" (not $ runTest example17)
      it "detects missing Fez(A, A) case" (not $ runTest example18)
      it "detects missing Fez(A, True) case" (not $ runTest example19)
      it "detects missing Fez(True, A) case" (not $ runTest example20)
