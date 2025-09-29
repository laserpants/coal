{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.PatternAnomaliesSpec (patternAnomaliesSpec) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Common.Label (Label (..))
import Coal.Compiler.Environment
import Coal.Compiler.PatternAnomalies
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Pattern (Pattern (..))
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Extra (Name)
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

testEnv :: Environment (DataConstructor TypeIndex Kind IndexedType, Set Name)
testEnv =
  Environment.fromList
    [
      ( "Cons"
      ,
        ( DataConstructor
            "Cons"
            2
            (Forall mempty [] (TApplication KType (TConstructor (KArrow KType KType) "List") (TVariable (TypeIndex KType 0) :| [])))
        , Set.fromList ["Cons", "Nil"]
        )
      )
    ,
      ( "Nil"
      ,
        ( DataConstructor
            "Nil"
            0
            (Forall mempty [] (TApplication KType (TConstructor (KArrow KType KType) "List") (TVariable (TypeIndex KType 0) :| [])))
        , Set.fromList ["Cons", "Nil"]
        )
      )
    ]

runTest :: [Pat] -> Bool
runTest px = res
 where
  Right res = runIdentity (evalCompilerT env (exhaustive px))
  env = emptyCompilerEnvironment{compilerDataConstructorEnvironment = testEnv}

example6 :: [Pattern () ()]
example6 =
  [ PConstructor () (Label () "Cons") [PVariable () (Label () "x"), PConstructor () (Label () "Cons") [PVariable () (Label () "y"), PVariable () (Label () "ys")]]
  , PConstructor () (Label () "Nil") []
  , PConstructor () (Label () "Cons") [PAny () (), PAny () ()]
  ]

example7 :: [Pattern () ()]
example7 =
  [ PConstructor () (Label () "Cons") [PVariable () (Label () "x"), PConstructor () (Label () "Cons") [PVariable () (Label () "y"), PVariable () (Label () "ys")]]
  , PConstructor () (Label () "Nil") []
  ]

example8 :: [Pattern () ()]
example8 =
  [ PConstructor () (Label () "Cons") [PVariable () (Label () "x"), PConstructor () (Label () "Cons") [PVariable () (Label () "y"), PVariable () (Label () "ys")]]
  , PConstructor () (Label () "Cons") [PAny () (), PAny () ()]
  ]

example9 :: [Pattern () ()]
example9 =
  [ PConstructor () (Label () "Cons") [PVariable () (Label () "x"), PConstructor () (Label () "Cons") [PVariable () (Label () "y"), PVariable () (Label () "ys")]]
  , POr
      ()
      ()
      (PConstructor () (Label () "Nil") [])
      (PConstructor () (Label () "Cons") [PAny () (), PAny () ()])
  ]

example10 :: [Pattern () ()]
example10 =
  [ PConstructor () (Label () "Cons") [PVariable () (Label () "x"), PConstructor () (Label () "Cons") [PVariable () (Label () "y"), PVariable () (Label () "ys")]]
  , PConstructor () (Label () "Cons") [PAny () (), PAny () ()]
  , PAny () ()
  ]

example11 :: [Pattern () ()]
example11 =
  [ PAny () ()
  ]

runTest2 :: [Pattern a t] -> Bool
runTest2 px = res
 where
  Right res = runIdentity (evalCompilerT env (exhaustive (translatePattern <$> px)))
  env = emptyCompilerEnvironment{compilerDataConstructorEnvironment = testEnv}

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
