{-# LANGUAGE StrictData #-}

module Coal.CLI.Options.CompileCmd (CompileCmdOptions (..)) where

import Data.Text (Text)

data CompileCmdOptions = CompileCmdOptions
  { inputFiles :: [FilePath]
  , outputFile :: FilePath
  , srcPaths :: [FilePath]
  , generateDebugArtifacts :: Bool
  , debugLLVMOutput :: Bool
  , extraSourceFiles :: [FilePath]
  , silent :: Bool
  , noCache :: Bool
  , entryPoint :: Maybe Text
  }
  deriving (Show)
