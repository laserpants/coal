{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.PatternAnomaliesSpec (patternAnomaliesSpec) where

import Coal.Compiler.PatternAnomalies
import Control.Monad.Reader
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

testEnv :: AnomaliesEnvironment
testEnv =
  anomaliesEnvironment
    [ ("Cons", ["Cons", "Nil"])
    , ("Nil", ["Cons", "Nil"])
    ]

runTest :: [Pat] -> Bool
runTest px = runReader (exhaustive px) testEnv

patternAnomaliesSpec :: Spec
patternAnomaliesSpec =
  describe "TODO" $ do
    it "example1" (runTest example1)
    it "example2" (not $ runTest example2)
    it "example3" (not $ runTest example3)
    it "example4" (runTest example4)
    it "example5" (runTest example5)
