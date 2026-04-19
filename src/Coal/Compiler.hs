{- | Public API for the Coal compiler

This module exposes only the essential compiler interface needed by
the CLI and testing infrastructure. Internal compiler modules are
not re-exported to maintain clear API boundaries.
-}
module Coal.Compiler (
  -- * Compilation
  compile,
  compileWithCFiles,
  pipeline,

  -- * Error handling
  prettyError,

  -- * Configuration
  CompilerConfig (..),
  defaultConfig,
) where

import Coal.Compiler.Config (CompilerConfig (..), defaultConfig)
import Coal.Compiler.Pipeline (compile, compileWithCFiles, pipeline, prettyError)
