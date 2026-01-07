{-# LANGUAGE StrictData #-}

module Coal.CLI.Options.CompileCmd (CompileCmdOptions (..)) where

data CompileCmdOptions = CompileCmdOptions
  { inputFiles :: [FilePath]
  , outputFile :: FilePath
  , srcPaths :: [FilePath]
  , debugGenerateDotfiles :: Bool
  , debugLLVMOutput :: Bool
  , extraSourceFiles :: [FilePath]
  , silent :: Bool
  , noCache :: Bool
  }
  deriving (Show)
