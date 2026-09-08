{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Temporary diagnostic tool (troubleshooting only): run each kernel
normalization pass in isolation over dumped kernel IR modules (produced by
@coal compile --generate-debug-artifacts@), reporting per-pass wall time, heap
allocation, and output size.

Usage:

> passharness FILE.coal ... [--only=SUBSTR] [+RTS -T]

With @--only=SUBSTR@, only top-level objects whose name contains SUBSTR are
kept, for function-level bisection. Run with @+RTS -T -RTS@ to enable
per-pass allocation accounting.
-}
module Main (main) where

import Coal.Kernel.Builtin.Objects (builtinObjects)
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Type (Type)
import qualified Coal.Kernel.Parser.Module as Parser
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
import qualified Coal.Kernel.Prettyprinter as NKPretty
import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import Control.Monad.Identity (Identity)
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Stats (RTSStats (..), getRTSStats, getRTSStatsEnabled)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import qualified Text.Megaparsec as MP

main :: IO ()
main = do
  args <- getArgs
  let (onlys, files) = partitionArgs args
  statsOn <- getRTSStatsEnabled
  unless statsOn $
    putStrLn "warning: run with +RTS -T to get per-pass allocation"
  forM_ files $ \f -> do
    src0 <- Text.readFile f
    -- The pretty-printer emits import lines that the kernel parser cannot
    -- re-read (e.g. "import Builtin$.(!=)"). Normalization never consults
    -- moduleImports, so strip them before parsing.
    let src = renameOps (Text.unlines (filter (not . Text.isPrefixOf "import") (Text.lines src0)))
    case MP.parse Parser.module_ f src of
      Left err -> do
        putStrLn (f ++ ": parse failed:\n" ++ MP.errorBundlePretty err)
        exitFailure
      Right m0 -> do
        let m = injectBuiltins (filterObjects onlys m0)
        putStrLn ("=== " ++ f ++ ": " ++ show (length (moduleObjects m)) ++ " objects, size " ++ show (moduleSize m))
        hFlush stdout
        runPasses m passList

-- | Replicate the builtin DData injection done by passKernelCodegen before it
-- calls 'Coal.Kernel.Compiler.compileModules'.
injectBuiltins :: Module Type -> Module Type
injectBuiltins m = m{moduleObjects = moduleObjects builtinObjects <> moduleObjects m}

partitionArgs :: [String] -> ([String], [FilePath])
partitionArgs = go [] []
  where
    go os fs [] = (reverse os, reverse fs)
    go os fs (a : as)
      | "--only=" `isPrefixOf` a = go (drop 7 a : os) fs as
      | otherwise = go os (a : fs) as

filterObjects :: [String] -> Module Type -> Module Type
filterObjects [] m = m
filterObjects subs m = m{moduleObjects = filter match (moduleObjects m)}
  where
    match obj = any (`isInfixOf` Text.unpack (objectName obj)) subs

objectName :: Object t -> Text.Text
objectName = \case
  DFunction _ n _ _ -> n
  DConstant n _ -> n
  DExternal n _ -> n
  DData n _ -> n

passList :: [(String, Pass Identity (Module Type) (Module Type))]
passList =
  [ ("case-canonicalization", caseExpressionCanonicalization)
  , ("name-canonicalization", localNameCanonicalization)
  , ("lambda-flattening", lambdaFlattening)
  , ("constructor-saturation", constructorSaturation)
  , ("lambda-lifting", lambdaLifting)
  , ("toplevel-fn-normalize", topLevelFunctionNormalization)
  , ("fn-result-saturation", functionResultsSaturation)
  , ("logical-op-translate", logicalOperatorTranslation)
  , ("let-simplification", letBindingSimplification)
  , ("anf", administrativeNormalForm)
  ]

-- | Run the passes sequentially, printing time, allocation delta and output
-- size after each. The output size computation also forces the module.
runPasses :: Module Type -> [(String, Pass Identity (Module Type) (Module Type))] -> IO ()
runPasses m0 passes = go 0 m0 passes
  where
    go _ _ [] = putStrLn "all passes done"
    go n m ((name, p) : rest) = do
      t0 <- getCurrentTime
      a0 <- allocatedBytes
      let step = case evalPipeline initialPipelineState (p m) of
            Left err -> error ("pass " ++ name ++ " failed: " ++ show err)
            Right m' -> m'
      size <- evaluate (moduleSize step)
      t1 <- getCurrentTime
      a1 <- allocatedBytes
      putStrLn
        ( "pass " ++ show n ++ " " ++ name
            ++ ": time " ++ show (realToFrac (diffUTCTime t1 t0) :: Double) ++ "s"
            ++ ", alloc " ++ show (a1 - a0) ++ " bytes"
            ++ ", size " ++ show size ++ " chars"
        )
      hFlush stdout
      go (n + 1) step rest

-- | Cumulative bytes allocated by the program so far (0 if RTS stats are off).
allocatedBytes :: IO Integer
allocatedBytes = do
  ok <- getRTSStatsEnabled
  if ok
    then do
      s <- getRTSStats
      pure (fromIntegral (allocated_bytes s))
    else pure 0

-- | Approximate module size: length of the pretty-printed IR.
moduleSize :: Module Type -> Int
moduleSize m = Text.length (NKPretty.renderModule m)

{- | Replace every parenthesized operator name @(<symbols>)@ (e.g. @(+)@,
@(==)@) with a legal kernel identifier @_zNN@ (NN = character codes), since
the kernel parser cannot re-read operator names in object headers, record
fields, projections, or type arguments. The mapping is purely textual and
consistent across the whole file, so all references stay in sync. -}
renameOps :: Text.Text -> Text.Text
renameOps = Text.pack . go . Text.unpack
  where
    go [] = []
    go ('(' : cs) =
      let (sym, rest) = span isSym cs
       in case rest of
            (')' : rest')
              | not (null sym) -> legalName sym ++ go rest'
            _ -> '(' : go cs
    go (c : cs) = c : go cs
    isSym c = c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)
    legalName = ('_' :) . concatMap (\c -> 'z' : show (fromEnum c))

