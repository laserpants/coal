{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
LLVM IR code generation from Coal kernel language modules.

The compiler translates kernel language modules (post-normalization) into LLVM IR.
It orchestrates:

  * Parsing and pipeline normalization
  * Code generation via the LLVM subsystem
  * Error accumulation and reporting

= Compilation flow

The typical compilation pipeline is:

  1. Parse source files to untyped modules
  2. Run normalization passes (see "Coal.Kernel.Pipeline")
  3. Generate LLVM IR via "Coal.Kernel.LLVM.Codegen"
  4. Emit IR module for linking or JIT execution

This module provides the high-level entry points ('compileModules',
'compileFiles') that orchestrate these steps.
-}
module Coal.Kernel.Compiler (
  -- * Compiler monad transformer
  CompilerT (..),
  Compiler,

  -- * Errors
  CompilerError (..),

  -- * Running
  runCompilerT,
  runCompiler,

  -- * Entry points
  compileModules,
  compileFiles,
) where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (runStateT)
import Control.Monad.Trans (MonadTrans (..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import LLVM.IR

import Coal.Compiler.Config (CompilerConfig (configEntryPoint))
import Coal.Kernel.LLVM.Codegen (irMainModule, irModule)
import Coal.Kernel.LLVM.Monad (IRCodegen, IRCodegenError, runIRCodegen)
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Type (Type)
import qualified Coal.Kernel.Parser.Module as Parser
import Coal.Kernel.Pipeline (PipelineError, evalPipeline, initialPipelineState)
import Coal.Kernel.Pipeline.Passes (pipeline)
import Text.Megaparsec (errorBundlePretty, parse)

-- ---------------------------------------------------------------------------
-- Error type
-- ---------------------------------------------------------------------------

{- | All errors that can arise during compilation, from source-file I/O
through parsing, normalization passes, and LLVM IR code generation.
-}
data CompilerError
  = -- | A source file failed to parse.  The string is the output of
    --     'errorBundlePretty', suitable for direct display to the user.
    CompilerParseError String
  | -- | A normalization pass signalled an error (e.g. over-saturated
    --     constructor application).
    CompilerPipelineError PipelineError
  | -- | The IR code-generation phase encountered a semantic error
    --     (e.g. unbound variable or non-function type in call position).
    CompilerIRCodegenError IRCodegenError
  | -- | The LLVM IR builder signalled an internal construction error
    --     (e.g. block already terminated).  These indicate a compiler bug
    --     rather than a user error.
    CompilerIRBuilderError IRBuilderError
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Monad transformer
-- ---------------------------------------------------------------------------

{- | 'CompilerT' is a thin transformer that threads a 'CompilerError' failure
channel through any base monad @m@.  The pipeline and IR-codegen phases each
have their own internal monad stacks; 'CompilerT' sequences them and converts
their typed errors into 'CompilerError'.
-}
newtype CompilerT m a = CompilerT
  { unCompilerT :: ExceptT CompilerError m a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError CompilerError
    , MonadIO
    )

instance MonadTrans CompilerT where
  lift = CompilerT . lift

-- | Specialization of 'CompilerT' to the pure 'Identity' base monad.
type Compiler = CompilerT Identity

-- ---------------------------------------------------------------------------
-- Runners
-- ---------------------------------------------------------------------------

{- | Unwrap a 'CompilerT' action, returning either a 'CompilerError' or a
successful result in the base monad @m@.
-}
runCompilerT :: CompilerT m a -> m (Either CompilerError a)
runCompilerT = runExceptT . unCompilerT

{- | Run a pure 'Compiler' action, extracting either a 'CompilerError' or a
successful result.
-}
runCompiler :: Compiler a -> Either CompilerError a
runCompiler = runIdentity . runCompilerT

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Run all normalization passes on a single module.
normalizeModule :: (Monad m) => Module Type -> CompilerT m (Module Type)
normalizeModule m =
  CompilerT $
    either (throwError . CompilerPipelineError) return $
      evalPipeline initialPipelineState (pipeline m)

{- | Run the LLVM IR builder and 'IRCodegen' action, producing an 'IRModule'
or a 'CompilerError'.
-}
buildIR :: IRCodegen IRModule -> Either CompilerError IRModule
buildIR action =
  case runIdentity $
    runExceptT $
      runStateT
        (runIRBuilder (runIRCodegen mempty action))
        (emptyIRBuilderEnv :: IRBuilderEnv) of
    Left builderErr ->
      Left (CompilerIRBuilderError builderErr)
    Right (Left codeGenErr, _) ->
      Left (CompilerIRCodegenError codeGenErr)
    Right (Right ir, _) ->
      Right ir

{- | Run LLVM IR code generation for one normalized module, given the full
list of all (normalized) modules for cross-module context.

For the entry point module (default: "Main"), additionally emits the C main
entry point via 'irMainModule'.
-}
codeGenModule :: (Monad m) => CompilerConfig -> [Module Type] -> Module Type -> CompilerT m IRModule
codeGenModule config allModules m =
  let isEntryPoint = case configEntryPoint config of
        Nothing -> moduleName m == Text.pack "Main"
        Just (entryMod, _) -> moduleName m == entryMod
      entryPointModule = case configEntryPoint config of
        Nothing -> Text.pack "Main"
        Just (entryMod, _) -> entryMod
      entryPointFunc = case configEntryPoint config of
        Nothing -> Text.pack "main"
        Just (_, entryFunc) -> entryFunc
   in CompilerT $
        either throwError return $
          if isEntryPoint
            then buildIR (irModule allModules m (irMainModule entryPointModule entryPointFunc))
            else buildIR (irModule allModules m (return ()))

-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

{- | Compile a list of modules through the full normalization pipeline
and LLVM IR code generator, producing one 'IRModule' per input module.

The pipeline is run purely (no IO required); the base monad @m@ is
unconstrained beyond 'Monad'.

Example:
@
  case runCompiler (compileModules modules) of
    Left  err -> putStrLn ("Compiler error: " <> show err)
    Right irs -> mapM_ (Text.putStrLn . runIRRenderer . renderModule) irs
@
-}
compileModules :: (Monad m) => CompilerConfig -> [Module Type] -> CompilerT m [IRModule]
compileModules config mods = do
  normalized <- traverse normalizeModule mods
  traverse (codeGenModule config normalized) normalized

{- | Read and parse source files from the given paths, then call
'compileModules'.

Parse errors for any file abort the compilation and are reported as
'CompilerParseError' (the bundled message from @megaparsec@'s
'errorBundlePretty').
-}
compileFiles :: CompilerConfig -> [FilePath] -> CompilerT IO [IRModule]
compileFiles config paths = do
  mods <- traverse parseOne paths
  compileModules config mods
 where
  parseOne path = do
    src <- liftIO (Text.readFile path)
    case parse Parser.module_ path src of
      Left bundle ->
        throwError (CompilerParseError (errorBundlePretty bundle))
      Right m ->
        return m
