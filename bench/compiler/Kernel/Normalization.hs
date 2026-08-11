{- |
Module: Kernel.Normalization
Description: Benchmarks for kernel IR normalization passes.

Each normalization pass is benchmarked in isolation on realistic kernel IR
inputs, allowing detection of performance regressions in specific transforms.
-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Kernel.Normalization (benchmarks) where

import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Pipeline (Pass, evalPipeline, initialPipelineState)
import Coal.Kernel.Pipeline.Pass.AdministrativeNormalForm (administrativeNormalForm)
import Coal.Kernel.Pipeline.Pass.CaseExpressionCanonicalization (caseExpressionCanonicalization)
import Coal.Kernel.Pipeline.Pass.ConstructorSaturation (constructorSaturation)
import Coal.Kernel.Pipeline.Pass.FunctionResultsSaturation (functionResultsSaturation)
import Coal.Kernel.Pipeline.Pass.LambdaFlattening (lambdaFlattening)
import Coal.Kernel.Pipeline.Pass.LambdaLifting (lambdaLifting)
import Coal.Kernel.Pipeline.Pass.LetBindingSimplification (letBindingSimplification)
import Coal.Kernel.Pipeline.Pass.LocalNameCanonicalization (localNameCanonicalization)
import Coal.Kernel.Pipeline.Pass.LogicalOperatorTranslation (logicalOperatorTranslation)
import Coal.Kernel.Pipeline.Pass.TopLevelFunctionNormalization (topLevelFunctionNormalization)
import Coal.Kernel.Pipeline.Passes (pipeline)
import Control.DeepSeq (NFData (..))
import Control.Monad.Identity (Identity)
import Criterion.Main
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Text.Megaparsec as MP
import qualified Coal.Kernel.Parser.Module as Parser

-- | All normalization benchmark groups.
benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "normalization" $
      concatMap (benchmarksForModule "small" "") (take 1 exampleModules)
        ++ concatMap (benchmarksForModule "medium" "") (drop 1 (take 2 exampleModules))
  ]
 where
  exampleModules = unsafeParseInputs exampleFiles
  exampleFiles =
    [ "test/examples/034/Main.corn"
    , "test/examples/002/Main.corn"
    , "test/examples/034/List.corn"
    ]

-- | A named parsed module ready for benchmarking.
data Input = Input
  { inputName :: String
  , inputMod :: Module Type
  }

-- | Parse inputs lazily (forced when benchmarks execute). Uses unsafePerformIO
-- to read files at benchmark definition time.
unsafeParseInputs :: [FilePath] -> [Input]
unsafeParseInputs files =
  [ Input (takeBaseName' f) (injectBuiltins m)
  | f <- files
  , let src = unsafeReadFile f
        m = case parseModule src of
              Left _ -> error ("Failed to parse: " ++ f)
              Right x -> x
  ]

-- | Read a file lazily using unsafePerformIO.
unsafeReadFile :: FilePath -> Text.Text
unsafeReadFile f = unsafePerformIO (Text.readFile f)

-- | Inject built-in DData declarations so kernel modules are self-contained.
injectBuiltins :: Module Type -> Module Type
injectBuiltins m = m{moduleObjects = builtinObjects <> moduleObjects m}

-- | Minimal built-in data type declarations needed by most kernel modules.
builtinObjects :: [Object Type]
builtinObjects =
  [ DData
      "List"
      [ ("$Cons", TCon "/" [TOpq, TCon "/" [TCon "List" [TOpq], TCon "List" [TOpq]]])
      , ("$Nil", TCon "List" [TOpq])
      ]
  , DData
      "record"
      [ ("$Record", TCon "/" [TOpq, TCon "record" [TOpq]])
      ]
  , DData
      "tuple2"
      [ ("$Tuple2", TCon "/" [TOpq, TCon "/" [TOpq, TCon "tuple2" [TOpq, TOpq]]])
      ]
  ]

-- | Run all normalization passes on a single input module.
benchmarksForModule :: String -> String -> Input -> [Benchmark]
benchmarksForModule size _prefix Input{inputName, inputMod} =
  let label = size ++ "/" ++ inputName
   in [ bench (label ++ "/pipeline") $ nf (runIdentPass pipeline) inputMod
      , bench (label ++ "/case-canonicalization") $ nf (runIdentPass caseExpressionCanonicalization) inputMod
      , bench (label ++ "/name-canonicalization") $ nf (runIdentPass localNameCanonicalization) inputMod
      , bench (label ++ "/lambda-flattening") $ nf (runIdentPass lambdaFlattening) inputMod
      , bench (label ++ "/constructor-saturation") $ nf (runIdentPass constructorSaturation) inputMod
      , bench (label ++ "/lambda-lifting") $ nf (runIdentPass lambdaLifting) inputMod
      , bench (label ++ "/toplevel-fn-normalize") $ nf (runIdentPass topLevelFunctionNormalization) inputMod
      , bench (label ++ "/fn-result-saturation") $ nf (runIdentPass functionResultsSaturation) inputMod
      , bench (label ++ "/logical-op-translate") $ nf (runIdentPass logicalOperatorTranslation) inputMod
      , bench (label ++ "/let-simplification") $ nf (runIdentPass letBindingSimplification) inputMod
      , bench (label ++ "/anf") $ nf (runIdentPass administrativeNormalForm) inputMod
      ]

-- | Run a pure Identity pass and extract the result (or error).
runIdentPass :: Pass Identity i o -> i -> o
runIdentPass pass input =
  case evalPipeline initialPipelineState (pass input) of
    Left err -> error ("Pass failed: " ++ show err)
    Right result -> result

-- | Parse a kernel IR module from source text.
parseModule :: Text.Text -> Either String (Module Type)
parseModule content =
  case MP.parse Parser.module_ "" content of
    Left err -> Left (MP.errorBundlePretty err)
    Right m -> Right m

-- | Strip directory and extension from a file path.
takeBaseName' :: FilePath -> String
takeBaseName' = reverse . drop 1 . dropWhile (/= '.') . reverse . takeFileName

takeFileName :: FilePath -> String
takeFileName = reverse . takeWhile (/= '/') . reverse

-- | Orphan NFData instance for Module Type.
-- Uses 'show' to force full evaluation of the module tree.
-- This is pragmatic but correct — serialization overhead is constant
-- across all benchmarks.
instance NFData (Module Type) where
  rnf m = rnf (show m)


