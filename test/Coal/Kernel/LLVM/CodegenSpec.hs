{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Coal.Kernel.LLVM.CodegenSpec
Description: Integration tests for LLVM code generation

This module defines the test suite for the Coal LLVM code generator,
running end-to-end compilation tests on example Coal programs.
-}
module Coal.Kernel.LLVM.CodegenSpec (spec) where

import Coal.Kernel.LLVM.IntegrationTestUtils (testCompilePipeline, testCompilePipelineWithC)
import System.Process (readProcess)
import Test.Hspec (Spec, describe, it, shouldBe)

-- | A test case consisting of source files and expected output.
data TestCase = TestCase [FilePath] String

{- | Collection of integration test cases.

Each test case specifies:
  1. A list of .corn source files to compile
  2. The expected output when the compiled program is executed
-}
testCases :: [TestCase]
testCases =
  [ TestCase
      [ "test/examples/001/My.Utilities.corn"
      , "test/examples/001/Data.List.corn"
      , "test/examples/001/Main.corn"
      ]
      "1"
  , TestCase
      [ "test/examples/002/List.corn"
      , "test/examples/002/Utils.corn"
      , "test/examples/002/Utils.Function.corn"
      , "test/examples/002/Tree.corn"
      , "test/examples/002/Ordering.corn"
      , "test/examples/002/Main.corn"
      ]
      "103"
  , TestCase
      ["test/examples/003/Main.corn"]
      "3"
  , TestCase
      [ "test/examples/004/Data.Tree.corn"
      , "test/examples/004/Main.corn"
      ]
      "3"
  , TestCase
      ["test/examples/005/Main.corn"]
      "6"
  , TestCase
      ["test/examples/006/Main.corn"]
      "6"
  , TestCase
      ["test/examples/007/Main.corn"]
      "Hello, world! 🚀"
  , TestCase
      ["test/examples/008/Main.corn"]
      "🤖🤖 Hello, world!"
  , TestCase
      ["test/examples/009/Main.corn"]
      "🎷"
  , TestCase
      ["test/examples/010/Main.corn"]
      "4.500000\n4"
  , TestCase
      ["test/examples/011/Main.corn"]
      "10"
  , TestCase
      [ "test/examples/012/Group.corn"
      , "test/examples/012/Main.corn"
      ]
      "133"
  , TestCase
      ["test/examples/013/Main.corn"]
      "3"
  , TestCase
      ["test/examples/014/Main.corn"]
      "9999999999999999999999999999999999999998"
  , TestCase
      [ "test/examples/015/BinarySearch.corn"
      , "test/examples/015/Core$.corn"
      , "test/examples/015/Ordered.corn"
      , "test/examples/015/Main.corn"
      ]
      "1"
  , TestCase
      ["test/examples/016/Main.corn"]
      "4"
  , TestCase
      [ "test/examples/017/Core$.corn"
      , "test/examples/017/Main.corn"
      ]
      "5"
  , TestCase
      [ "test/examples/018/Core$.corn"
      , "test/examples/018/Main.corn"
      ]
      "5"
  , TestCase
      [ "test/examples/029/Main.corn"
      ]
      "120"
  , TestCase
      [ "test/examples/030/Main.corn"
      ]
      "120"
  , TestCase
      [ "test/examples/031/Main.corn"
      ]
      "1"
  , TestCase
      [ "test/examples/032/Main.corn"
      ]
      "1"
  , TestCase
      [ "test/examples/033/Main.corn"
      ]
      "3"
  , TestCase
      [ "test/examples/034/Function.corn"
      , "test/examples/034/List.corn"
      , "test/examples/034/Main.corn"
      , "test/examples/034/Ord.corn"
      , "test/examples/034/Tree.corn"
      ]
      "2"
  , TestCase
      [ "test/examples/035/Function.corn"
      , "test/examples/035/List.corn"
      , "test/examples/035/Main.corn"
      , "test/examples/035/Ord.corn"
      , "test/examples/035/Tree.corn"
      , "test/examples/035/Data/Option.corn"
      ]
      "2"
  , TestCase
      [ "test/examples/037/Main.corn"
      ]
      "5\n30\n10\n4\n-1"
  , TestCase
      [ "test/examples/038/Main.corn"
      , "test/examples/038/Data/Map.corn"
      , "test/examples/038/Data/Option.corn"
      , "test/examples/038/Data/Ord.corn"
      ]
      "5\n30\n10\n4\n-1"
  , TestCase
      [ "test/examples/039/Main.corn"
      , "test/examples/039/Data/Map.corn"
      , "test/examples/039/Data/Option.corn"
      , "test/examples/039/Data/Ord.corn"
      , "test/examples/039/Data/Set.corn"
      ]
      "1\n5\n1\n0\n4\n0"
  , TestCase
      [ "test/examples/041/Main.corn"
      ]
      "12334"
  , TestCase
      [ "test/examples/042/Main.corn"
      ]
      "1239"
  , TestCase
      [ "test/examples/043/Main.corn"
      ]
      "8"
  , TestCase
      [ "test/examples/044/Main.corn"
      ]
      "8.000000"
  , TestCase
      [ "test/examples/045/Main.corn"
      ]
      "7.000000\n16.000000"
  , TestCase
      [ "test/examples/046/Main.corn"
      ]
      "0"
  , TestCase
      [ "test/examples/047/Main.corn"
      ]
      "5"
  , TestCase
      [ "test/examples/048/Main.corn"
      ]
      "0"
  , TestCase
      [ "test/examples/049/Main.corn"
      ]
      "5"
  ]

-- | Run a single test case: compile the files and check the output.
runTestCase :: TestCase -> IO String
runTestCase (TestCase files _expected) = do
  testCompilePipeline files
  actual <- readProcess ".build/dist" [] ""
  let trimmed = reverse (dropWhile (== '\n') (reverse actual))
  return trimmed

-- | HSpec test suite for LLVM code generation.
spec :: Spec
spec = do
  describe "End-to-end compilation" $ do
    mapM_ makeTest testCases
  describe "Compile-only (interactive programs)" $ do
    it "compiles example 036 (event loop with C runtime)" $
      testCompilePipelineWithC
        ["test/examples/036/Main.corn"]
        ["test/examples/036/runtime.c"]
 where
  makeTest (TestCase files expected) =
    it ("compiles and runs: " <> show files) $ do
      actual <- runTestCase (TestCase files expected)
      actual `shouldBe` expected
