{- |
Module: Main
Description: Entry point for Coal compiler performance benchmarks.

This benchmark suite measures the performance of the Coal compiler itself,
focusing on kernel normalization passes and compiler pipeline phases.

Usage:
  stack bench                                       # Run all benchmarks
  stack bench --ba='--output report.html'           # HTML report
  stack bench --ba='--match "normalization"'         # Filter by name
-}
module Main (main) where

import Criterion.Main
import qualified Kernel.Normalization as Normalization

main :: IO ()
main =
  defaultMain
    [ bgroup "kernel" Normalization.benchmarks
    ]
