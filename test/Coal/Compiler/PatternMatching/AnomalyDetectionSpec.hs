{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.PatternMatching.AnomalyDetectionSpec (patternAnomaliesSpec) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Common.Label (Label (..))
import Coal.Compiler.Build
import Coal.Compiler.Environment
import Coal.Compiler.PatternMatching.AnomalyDetection
import Coal.Compiler.Stack
import Coal.Language
import Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry
import Control.Monad.Identity (runIdentity)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Extras (Name)
import Test.Hspec

example1 :: [Pat]
example1 =
  [ Con "Cons" [Any, Con "Cons" [Any, Any]]
  , Con "Nil" []
  , Con "Cons" [Any, Any]
  ]

example2 :: [Pat]
example2 =
  [ Con "Cons" [Any, Con "Cons" [Any, Any]]
  , Con "Nil" []
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
  [ Con "Fez" [Lit (LBool False), Con "A" []]
  , Con "Fez" [Lit (LBool True), Con "B" []]
  ]

example14 :: [Pat]
example14 =
  [ Con "Fez" [Lit (LBool False), Con "A" []]
  , Con "Fez" [Lit (LBool True), Any]
  ]

example15 :: [Pat]
example15 =
  [ Con "Fez" [Lit (LBool False), Any]
  , Con "Fez" [Lit (LBool True), Con "A" []]
  ]

example16 :: [Pat]
example16 =
  [ Con "Fez" [Con "A" [], Con "A" []]
  , Con "Fez" [Con "B" [], Any]
  ]

example17 :: [Pat]
example17 =
  [ Con "Fez" [Con "A" [], Con "A" []]
  , Con "Fez" [Con "B" [], Con "B" []]
  ]

example18 :: [Pat]
example18 =
  [ Con "Fez" [Con "A" [], Any]
  , Con "Fez" [Con "B" [], Con "A" []]
  ]

example19 :: [Pat]
example19 =
  [ Con "Fez" [Con "A" [], Lit (LBool False)]
  , Con "Fez" [Con "B" [], Lit (LBool True)]
  ]

example20 :: [Pat]
example20 =
  [ Con "Fez" [Lit (LBool False), Con "A" []]
  , Con "Fez" [Lit (LBool False), Con "B" []]
  ]

testEnv :: Environment (ProtoDataConstructorEntry Metadata)
testEnv =
  Environment.fromList
    [
      ( "Cons"
      , ProtoDataConstructorEntry
          mempty
          "Cons"
          ( DataConstructor
              "Cons"
              2
              (Forall mempty [] (applyTypeArgs KType (TConstructor (KArrow KType KType) "List") (TVariable (TypeIndex KType 0) :| [])))
          )
          (Set.fromList ["Cons", "Nil"])
      )
    ,
      ( "Nil"
      , ProtoDataConstructorEntry
          mempty
          "Nil"
          ( DataConstructor
              "Nil"
              0
              (Forall mempty [] (applyTypeArgs KType (TConstructor (KArrow KType KType) "List") (TVariable (TypeIndex KType 0) :| [])))
          )
          (Set.fromList ["Cons", "Nil"])
      )
    ,
      ( "A"
      , ProtoDataConstructorEntry
          mempty
          "A"
          ( DataConstructor
              "A"
              0
              (Forall mempty [] (TConstructor KType "X"))
          )
          (Set.fromList ["A", "B"])
      )
    ,
      ( "B"
      , ProtoDataConstructorEntry
          mempty
          "B"
          ( DataConstructor
              "B"
              0
              (Forall mempty [] (TConstructor KType "X"))
          )
          (Set.fromList ["A", "B"])
      )
    ,
      ( "Fez"
      , ProtoDataConstructorEntry
          mempty
          "Fez"
          ( DataConstructor
              "Fez"
              2
              (Forall mempty [] (TIntrinsic IBool `TArrow` TConstructor KType "X" `TArrow` TConstructor KType "Fez"))
          )
          (Set.fromList ["Fez"])
      )
    ]

runTest :: [Pat] -> Bool
runTest px = res
 where
  Right res = runIdentity (evalCompilerT env (exhaustive px))
  env = (emptyCompilerEnvironment Nothing){compilerDataConstructorEnvironment = testEnv}

example6 :: [Pattern () () ()]
example6 =
  [ PConstructor () (Label () "Cons") [PVariable () (Label () "x"), PConstructor () (Label () "Cons") [PVariable () (Label () "y"), PVariable () (Label () "ys")]]
  , PConstructor () (Label () "Nil") []
  , PConstructor () (Label () "Cons") [PAny () (), PAny () ()]
  ]

example7 :: [Pattern () () ()]
example7 =
  [ PConstructor () (Label () "Cons") [PVariable () (Label () "x"), PConstructor () (Label () "Cons") [PVariable () (Label () "y"), PVariable () (Label () "ys")]]
  , PConstructor () (Label () "Nil") []
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
      (PConstructor () (Label () "Nil") [])
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
runTest2 px = res
 where
  Right res = runIdentity (evalCompilerT env (exhaustive (translatePattern <$> px)))
  env = (emptyCompilerEnvironment Nothing){compilerDataConstructorEnvironment = testEnv}

patternAnomaliesSpec :: Spec
patternAnomaliesSpec =
  describe "PatternAnomalies" $ do
    it "example1" (runTest example1)
    it "example2" (not $ runTest example2)
    it "example3" (not $ runTest example3)
    it "example4" (runTest example4)
    it "example5" (runTest example5)
    it "example6" (runTest2 example6)
    it "example7" (not $ runTest2 example7)
    it "example8" (not $ runTest2 example8)
    it "example9" (runTest2 example9)
    it "example10" (runTest2 example10)
    it "example11" (runTest2 example11)
    it "example12" (runTest example12)
    it "example13" (not $ runTest example13)
    it "example14" (not $ runTest example14)
    it "example15" (not $ runTest example15)
    it "example16" (not $ runTest example16)
    it "example17" (not $ runTest example17)
    it "example18" (not $ runTest example18)
    it "example19" (not $ runTest example19)
    it "example20" (not $ runTest example20)
