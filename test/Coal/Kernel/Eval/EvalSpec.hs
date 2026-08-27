{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Eval.EvalSpec (spec) where

import Coal.Kernel.Eval
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Test.Hspec

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

{- | Build an ExternTable that records each call to coal_print_int32 in an
IORef rather than writing to stdout, so tests can inspect the output.
-}
captureExterns :: IORef [String] -> ExternTable
captureExterns ref =
  Map.fromList
    [
      ( "coal_print_int32"
      , \case
          [VInt32 n] -> do
            modifyIORef ref (++ [show n])
            return (Right VUnit)
          args ->
            return (Left (ArityMismatch "coal_print_int32" 1 (length args)))
      )
    ,
      ( "coal_print_int64"
      , \case
          [VInt64 n] -> do
            modifyIORef ref (++ [show n])
            return (Right VUnit)
          args ->
            return (Left (ArityMismatch "coal_print_int64" 1 (length args)))
      )
    ,
      ( "coal_println_int32"
      , \case
          [VInt32 n] -> do
            modifyIORef ref (++ [show n])
            return (Right VUnit)
          args ->
            return (Left (ArityMismatch "coal_println_int32" 1 (length args)))
      )
    ]

{- | Parse and evaluate Main.main with [VUnit] across the given source files.
Checks that the side-effect output and the return value match expectations.
-}
checkMain :: [FilePath] -> [String] -> String -> Expectation
checkMain files expectedOutput expectedResult = do
  ref <- newIORef []
  result <- evalFunctionFromFiles (captureExterns ref) files "Main.main" [VUnit]
  output <- readIORef ref
  output `shouldBe` expectedOutput
  case result of
    Left parseErr ->
      expectationFailure $ "Parse error: " <> parseErr
    Right (Left evalErr) ->
      expectationFailure $ "Eval error: " <> show evalErr
    Right (Right val) ->
      showValue val `shouldBe` expectedResult

-- ---------------------------------------------------------------------------
-- Test suite
-- ---------------------------------------------------------------------------

spec :: Spec
spec =
  describe "Coal.Kernel.Eval" $ do
    it "003/Main.corn: tree height = 3, cont returns 0" $
      checkMain
        ["test/examples/normal/003/Main.corn"]
        ["3"]
        "0"

    it "005/Main.corn: partial application result = 6, cont returns 0" $
      checkMain
        ["test/examples/normal/005/Main.corn"]
        ["6"]
        "0"

    it "019/Main.corn: factorial(6) = 720, cont returns 0" $
      checkMain
        ["test/examples/normal/019/Main.corn"]
        ["720"]
        "0"

    it "020/Main.corn: record field baz = 456, cont returns 0" $
      checkMain
        ["test/examples/normal/020/Main.corn"]
        ["456"]
        "0"

    it "021/Main.corn + Foo.corn: Foo.foo() = 123, cont returns 0" $
      checkMain
        [ "test/examples/normal/021/Main.corn"
        , "test/examples/normal/021/Foo.corn"
        ]
        ["123"]
        "0"

    it "022/Main.corn: letrec self-reference in simple lambda = 3, cont returns 0" $
      checkMain
        ["test/examples/normal/022/Main.corn"]
        ["3"]
        "0"

    it "023/Main.corn: letrec self-reference in nested lambda = 60, cont returns 0" $
      checkMain
        ["test/examples/normal/023/Main.corn"]
        ["60"]
        "0"

    it "024/Main.corn: process same list twice with different filters = 4, cont returns 0" $
      checkMain
        ["test/examples/normal/024/Main.corn"]
        ["4"]
        "0"

    it "normal/002: BST sort of [2,105,103,104,2,106] has 103 as 3rd element" $
      checkMain
        [ "test/examples/normal/002/Ordering.corn"
        , "test/examples/normal/002/Utils.corn"
        , "test/examples/normal/002/Utils.Function.corn"
        , "test/examples/normal/002/List.corn"
        , "test/examples/normal/002/Tree.corn"
        , "test/examples/normal/002/Main.corn"
        ]
        ["103"]
        "103"

    -- Regression tests for isConstructorName: $-prefixed constructors must be
    -- recognised as constructors, not variable binders, during pattern matching.
    it "normal/025: head_or_zero([77]) = 77 ($Nil-first case, non-empty scrutinee)" $
      checkMain
        ["test/examples/normal/025/Main.corn"]
        ["77"]
        "0"

    it "normal/026: head_or_99([]) = 99 ($Nil-first case, empty scrutinee)" $
      checkMain
        ["test/examples/normal/026/Main.corn"]
        ["99"]
        "0"

    it "normal/027: list_sum([1,2,3]) = 6 (recursive $Nil-first, $Cons branch must recurse)" $
      checkMain
        ["test/examples/normal/027/Main.corn"]
        ["6"]
        "0"

    it "029/Main.corn: product([1,2,3,4,5]) = 120, cont returns 0" $
      checkMain
        ["test/examples/029/Main.corn"]
        ["120"]
        "0"

    it "030/Main.corn: product([1,2,3,4,5]) = 120, cont returns 0" $
      checkMain
        ["test/examples/030/Main.corn"]
        ["120"]
        "0"

    it "031/Main.corn: head(sort([5,2,1,4,3])) = 1, cont returns 0" $
      checkMain
        ["test/examples/031/Main.corn"]
        ["1"]
        "0"

    it "032/Main.corn: head(sort([5,2,1,4,3])) = 1, cont returns 0" $
      checkMain
        ["test/examples/032/Main.corn"]
        ["1"]
        "0"

    it "033/Main.corn: head(sort([7,4,3,6,5])) = 3, cont returns 0" $
      checkMain
        ["test/examples/033/Main.corn"]
        ["3"]
        "0"

    it "034/Main.corn: head(sort([7,4,2,6,5])) = 2, cont returns 0" $
      checkMain
        [ "test/examples/034/Function.corn"
        , "test/examples/034/List.corn"
        , "test/examples/034/Main.corn"
        , "test/examples/034/Ord.corn"
        , "test/examples/034/Tree.corn"
        ]
        ["2"]
        "0"

    it "035/Main.corn: head(sort([7,4,2,6,5])) = 2, cont returns 0" $
      checkMain
        [ "test/examples/035/Function.corn"
        , "test/examples/035/List.corn"
        , "test/examples/035/Main.corn"
        , "test/examples/035/Ord.corn"
        , "test/examples/035/Tree.corn"
        , "test/examples/035/Data/Option.corn"
        ]
        ["2"]
        "0"

    it "037/Main.corn: 5\n30\n10\n4\n-1, cont returns 0" $
      checkMain
        [ "test/examples/037/Main.corn"
        ]
        ["5", "30", "10", "4", "-1"]
        "0"

    it "038/Main.corn: 5\n30\n10\n4\n-1, cont returns 0" $
      checkMain
        [ "test/examples/038/Main.corn"
        , "test/examples/038/Data/Map.corn"
        , "test/examples/038/Data/Option.corn"
        , "test/examples/038/Data/Ord.corn"
        ]
        ["5", "30", "10", "4", "-1"]
        "0"

    it "039/Main.corn: 1\n5\n1\n0\n4\n0, cont returns 0" $
      checkMain
        [ "test/examples/039/Main.corn"
        , "test/examples/039/Data/Map.corn"
        , "test/examples/039/Data/Option.corn"
        , "test/examples/039/Data/Ord.corn"
        , "test/examples/039/Data/Set.corn"
        ]
        ["1", "5", "1", "0", "4", "0"]
        "0"

    it "041/Main.corn" $
      checkMain
        ["test/examples/041/Main.corn"]
        ["12334"]
        "0"

    it "042/Main.corn" $
      checkMain
        ["test/examples/042/Main.corn"]
        ["1239"]
        "0"

    it "043/Main.corn" $
      checkMain
        ["test/examples/043/Main.corn"]
        ["8"]
        "0"
