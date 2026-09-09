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

  -- * Per-module code generation
  codeGenModule,
) where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (runStateT)
import Control.Monad.Trans (MonadTrans (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import LLVM.IR
import LLVM.IRRenderer (renderModule)

import Coal.Common.Name (Name)
import Coal.Compiler.Config (CompilerConfig (configEntryPoint))
import Coal.Kernel.LLVM.Codegen (irMainModule, irModule)
import Coal.Kernel.LLVM.Monad (IRCodegen, IRCodegenEnv (..), IRCodegenError, runIRCodegen)
import Coal.Kernel.Language.Interface (ObjectInterface)
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Type (Type)
import qualified Coal.Kernel.Parser.Module as Parser
import Coal.Kernel.Pipeline (PipelineError, evalPipeline, initialPipelineState)
import Coal.Kernel.Pipeline.Pass.AdministrativeNormalForm (administrativeNormalForm)
import Coal.Kernel.Pipeline.Pass.FunctionResultsSaturation (functionResultsSaturation)
import Coal.Kernel.Pipeline.Pass.LambdaLifting (lambdaLifting)
import Coal.Kernel.Pipeline.Pass.LetBindingSimplification (letBindingSimplification)
import Coal.Kernel.Pipeline.Pass.LogicalOperatorTranslation (logicalOperatorTranslation)
import Coal.Kernel.Pipeline.Pass.TopLevelFunctionNormalization (topLevelFunctionNormalization)
import qualified Coal.Kernel.Pipeline.Passes as Passes (structuralNorm)
import qualified Coal.Kernel.Prettyprinter as NKPretty
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (errorBundlePretty, parse)

-- ---------------------------------------------------------------------------
-- Error type
-- ---------------------------------------------------------------------------

{- | All errors that can arise during compilation, from source-file I/O
through parsing, normalization passes, and LLVM IR code generation.
-}
data CompilerError
  = {- | A source file failed to parse.  The string is the output of
    'errorBundlePretty', suitable for direct display to the user.
    -}
    CompilerParseError String
  | {- | A normalization pass signalled an error (e.g. over-saturated
    constructor application).
    -}
    CompilerPipelineError PipelineError
  | {- | The IR code-generation phase encountered a semantic error
    (e.g. unbound variable or non-function type in call position).
    -}
    CompilerIRCodegenError IRCodegenError
  | {- | The LLVM IR builder signalled an internal construction error
    (e.g. block already terminated).  These indicate a compiler bug
    rather than a user error.
    -}
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

{- | Run all normalization passes on a single module.

TEMPORARY DEBUG INSTRUMENTATION (kernel allocation-storm investigation):
runs each normalization pass individually and traces wall time and
pretty-printed output size per pass. Revert to the plain 'pipeline' run
once the investigation is complete.
-}
normalizeModule :: (Monad m) => Module Type -> CompilerT m (Module Type)
normalizeModule m = do
  m1 <- step "structuralNorm" Passes.structuralNorm m
  m2 <- step "lambdaLifting" lambdaLifting m1
  m3 <- step "topLevelFnNorm" topLevelFunctionNormalization m2
  m4 <- step "fnResultSat" functionResultsSaturation m3
  m5 <- step "logicalOpTrans" logicalOperatorTranslation m4
  m6 <- step "letBindSimp" letBindingSimplification m5
  m7 <- step "anf" administrativeNormalForm m6
  pure m7
 where
  step name p input = do
    let t0 = unsafePerformIO getCurrentTime
    t0 `seq` return ()
    out <- case evalPipeline initialPipelineState (p input) of
      Left err -> throwError (CompilerPipelineError err)
      Right o -> return o
    let sz = Text.length (NKPretty.renderModule out)
        t1 = unsafePerformIO getCurrentTime
        dt = realToFrac (diffUTCTime t1 t0) :: Double
        report =
          "[norm] "
            ++ Text.unpack (moduleName m)
            ++ " pass="
            ++ name
            ++ " time="
            ++ show dt
            ++ "s size="
            ++ show sz
    t0 `seq` t1 `seq` sz `seq` return (trace report out)

{- | Run the LLVM IR builder and 'IRCodegen' action with a given initial
environment, producing an 'IRModule' or a 'CompilerError'.
-}
buildIR :: IRCodegenEnv -> IRCodegen IRModule -> Either CompilerError IRModule
buildIR initEnv action =
  case runIdentity $
    runExceptT $
      runStateT
        (runIRBuilder (runIRCodegen initEnv action))
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
codeGenModule ::
  (Monad m) =>
  CompilerConfig ->
  Map Name Int ->
  Map Name Int ->
  Map Name ObjectInterface ->
  [Module Type] ->
  Module Type ->
  CompilerT m IRModule
codeGenModule config extraTags cachedDData cachedObjects allModules m =
  CompilerT $ do
    let t0 = unsafePerformIO getCurrentTime
    t0 `seq` return ()
    let action =
          if isEntryPoint
            then irModule allModules m (irMainModule entryPointModule entryPointFunc)
            else irModule allModules m (return ())
        res0 = buildIR initEnv action
    case res0 of
      Left e -> throwError e
      Right ir ->
        let l = Text.length (renderModule ir)
            t1 = unsafePerformIO getCurrentTime
            dt = realToFrac (diffUTCTime t1 t0) :: Double
            report =
              "[codegen] "
                ++ Text.unpack (moduleName m)
                ++ " time="
                ++ show dt
                ++ "s irLen="
                ++ show l
         in t0 `seq` t1 `seq` l `seq` return (trace report ir)
 where
  isEntryPoint =
    case configEntryPoint config of
      Nothing -> moduleName m == Text.pack "Main"
      Just (entryMod, _) -> moduleName m == entryMod
  entryPointModule =
    case configEntryPoint config of
      Nothing -> Text.pack "Main"
      Just (entryMod, _) -> entryMod
  entryPointFunc =
    case configEntryPoint config of
      Nothing -> Text.pack "main"
      Just (_, entryFunc) -> entryFunc
  initEnv =
    IRCodegenEnv
      { codegenVarEnv = mempty
      , codegenTagEnv = extraTags
      , codegenImportedDData = cachedDData
      , codegenImportedObjects = cachedObjects
      }

-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

{- | Compile a list of modules through the full normalization pipeline
and LLVM IR code generator, producing the normalized modules and one
'IRModule' per input module.

The pipeline is run purely (no IO required); the base monad @m@ is
unconstrained beyond 'Monad'.

Example:
@
  case runCompiler (compileModules modules) of
    Left  err -> putStrLn (\"Compiler error: \" <> show err)
    Right (_, irs) -> mapM_ (Text.putStrLn . runIRRenderer . renderModule) irs
@
-}
compileModules :: (Monad m) => CompilerConfig -> Map Name Int -> Map Name Int -> Map Name ObjectInterface -> [Module Type] -> CompilerT m ([Module Type], [IRModule])
compileModules config extraTags cachedDData cachedObjects mods = do
  normalized <- traverse normalizeModule mods
  irs <- traverse (codeGenModule config extraTags cachedDData cachedObjects normalized) normalized
  pure (normalized, irs)

{- | Read and parse source files from the given paths, then call
'compileModules'.

Parse errors for any file abort the compilation and are reported as
'CompilerParseError' (the bundled message from @megaparsec@'s
'errorBundlePretty').
-}
compileFiles :: CompilerConfig -> [FilePath] -> CompilerT IO [IRModule]
compileFiles config paths = do
  mods <- traverse parseOne paths
  snd <$> compileModules config Map.empty Map.empty Map.empty mods
 where
  parseOne path = do
    src <- liftIO (Text.readFile path)
    case parse Parser.module_ path src of
      Left bundle ->
        throwError (CompilerParseError (errorBundlePretty bundle))
      Right m ->
        return m
