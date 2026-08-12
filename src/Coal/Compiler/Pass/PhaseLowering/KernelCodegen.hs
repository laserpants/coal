{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- | New-kernel LLVM codegen pass.

Drives 'Coal.Kernel.Compiler.compileModules' on the translated kernel
modules, renders each 'IRModule' to LLVM assembly text, and assembles
it to bitcode via @llvm-as@.
-}
module Coal.Compiler.Pass.PhaseLowering.KernelCodegen (passKernelCodegen) where

import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build (Build (..))
import Coal.Compiler.Build.Cache (buildCacheDir, writeBuildFile)
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Build.NameEntry (DataConstructorEntry (..), TypeConstructorEntry (..))
import Coal.Compiler.Config (CompilerConfig (..))
import Coal.Compiler.Error (CompilerFailureMode (..))
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT, getBuildC, setBitcodeC)
import Coal.Compiler.State (CompilerState (compilerConfig))
import Coal.Debug (writeDebugFile)
import qualified Coal.Kernel.Builtin.Objects as Builtin
import qualified Coal.Kernel.Compiler as Kernel
import Coal.Kernel.Language.Module (Module (..))
import qualified Coal.Kernel.Language.Object as Kernel
import qualified Coal.Kernel.Language.Type as Kernel
import qualified Coal.Kernel.Language.Type.Constructors as Kernel
import qualified Coal.Kernel.Prettyprinter as NKPretty
import Coal.Language.Data.Constructor (DataConstructor (..))
import Coal.Language.Module.Path (principalPath)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Extras (Name)
import LLVM.IRModule (IRModule)
import LLVM.IRRenderer (renderModule)
import System.Exit (ExitCode (..))
import System.FilePath ((<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (proc)
import qualified System.Process.ByteString as ProcessBS
import TextShow (showt)

passKernelCodegen ::
  (MonadIO m) =>
  Pass Metadata m [BuildEnvelope (Module Kernel.Type)] [(Name, ByteString)]
passKernelCodegen = Pass{runPass = pass}

pass ::
  (MonadIO m) =>
  [BuildEnvelope (Module Kernel.Type)] ->
  CompilerT Metadata m [(Name, ByteString)]
pass envelopes = do
  config <- gets compilerConfig
  let CompilerConfig{configGenerateLLVMOutput, configGenerateDebugArtifacts} = config

  -- Separate cached modules (already bitcode) from source modules.
  let sources = [(moduleName m, m) | BSource m <- envelopes]
      cached =
        [ (principalPath (buildPath b), bc)
        | BCached b <- envelopes
        , bc <- foldMap pure (buildBitcode b)
        ]

  -- Optionally dump pretty-printed NK modules for debugging.
  when configGenerateDebugArtifacts $
    liftIO $
      forM_ sources $ \(name, m) ->
        writeDebugFile
          (".debug" </> "Kernel" </> Text.unpack name <.> "coal")
          (NKPretty.renderModule m)

  -- Inject built-in constructor DData into every source module so the LLVM
  -- codegen emits the required struct type declarations and make_% functions.
  -- Also inject into the Builtin$ module itself, since it uses $Cons/$Nil etc.
  let injectDData m = m{moduleObjects = builtinDData <> moduleObjects m}
      builtinMod = injectDData Builtin.builtinObjects
      augmented = injectDData . snd <$> sources

  -- Reconstruct data constructor tag bindings for BCached modules.
  -- Cached builds store constructors in declaration order via buildTypeConstructors,
  -- which matches the [0..] indexing used in kernel codegen's allTagBindings.
  -- Seeding the initial IRCodegenEnv with these tags allows BSource modules to
  -- pattern-match on constructors from BCached modules without UnboundVariable errors.
  let cachedTagBindings =
        Map.fromList
          [ (principalPath (buildPath b) <> "." <> ctorName, idx)
          | BCached b <- envelopes
          , (_, TypeConstructorEntry{typeConstructorEntryDataConstructors = ctors}) <-
              Environment.toList (buildTypeConstructors b)
          , (idx, ctorName) <- zip [0 ..] (sort ctors)
          ]

  -- Reconstruct constructor field counts for BCached modules.
  -- Needed so that irModule can emit sized LLVM struct type declarations
  -- (via irImportedDataConstructor) for constructors that live in cached builds
  -- and are referenced by freshly compiled BSource modules.
  let cachedDDataInfo =
        Map.fromList
          [ (principalPath (buildPath b) <> "." <> dataConstructorEntryName entry, constructorArity (dataConstructorEntryConstructor entry))
          | BCached b <- envelopes
          , (_, entry) <- Environment.toList (buildDataConstructors b)
          ]

  -- Run the new-kernel compiler purely on all source modules together
  -- (cross-module context is required for LLVM codegen).
  irs <- case Kernel.runCompiler (Kernel.compileModules config cachedTagBindings cachedDDataInfo (builtinMod : augmented)) of
    Left err -> do
      liftIO $ putStrLn ("[KernelCodegen] compilation failed:\n" <> show err)
      throwError CompilerError
    Right irs ->
      pure irs

  -- Assemble each module's LLVM IR to bitcode via llvm-as.
  -- irs[0] is Builtin$'s IR; irs[1..n] correspond to augmented[0..n-1].
  named <- case irs of
    (builtinIR : sourceIRs) ->
      pure (("Builtin$", builtinIR) : zip (fst <$> sources) sourceIRs)
    [] ->
      error "KernelCodegen: compileModules returned empty IR list"
  results <- liftIO $
    withSystemTempDirectory "coal-build-nk" $ \tmpDir ->
      traverse (assembleOne configGenerateLLVMOutput tmpDir) named

  case sequence results of
    Left ex -> do
      liftIO $ putStrLn ("llvm-as failed: " <> show ex)
      throwError CompilerError
    Right assembled -> do
      -- Persist build artifacts to the .build/ cache for incremental compilation.
      -- Skip Builtin$ (first entry in assembled), write only user source modules.
      forM_ (zip (fst <$> sources) (snd <$> drop 1 assembled)) $ \(name, bc) -> do
        setBitcodeC name bc
        getBuildC name >>= \case
          Nothing -> pure ()
          Just build_ -> writeBuildFile buildCacheDir name build_
      pure (assembled <> cached)

{- | Render one IR module to text, optionally write a debug @.ll@ file,
then call @llvm-as@ to produce bitcode.
-}
assembleOne ::
  Bool ->
  FilePath ->
  (Name, IRModule) ->
  IO (Either SomeException (Name, ByteString))
assembleOne writeDebug tmpDir (name, ir) = do
  let llText = renderModule ir
      llFile = tmpDir </> nameToPath name <.> "ll"
  Text.writeFile llFile llText
  when writeDebug $
    writeDebugFile ("./.debug/" <> nameToPath name <.> "ll") llText
  fmap (name,) <$> runLLVMAs llFile

runLLVMAs :: FilePath -> IO (Either SomeException ByteString)
runLLVMAs src =
  try $ do
    (exit, out, err) <-
      ProcessBS.readCreateProcessWithExitCode
        (proc "llvm-as" [src, "-o", "-"])
        ""
    case exit of
      ExitSuccess ->
        pure out
      ExitFailure _ ->
        error $
          "llvm-as failed:\n"
            <> (if BS.null err then "<no stderr>" else show err)

{- | Convert a qualified module name like @Foo.Bar@ to @Foo_Bar@ for use in
temporary file names.
-}
nameToPath :: Name -> String
nameToPath = fmap (\c -> if c == '.' then '_' else c) . Text.unpack

{- | Built-in constructor 'DData' objects injected into every source module.

Distinct from 'Coal.Kernel.Builtin.Objects.builtinObjects' (the full builtin
function module): this list only provides the struct type declarations and
@make_%@ constructor functions for built-in data constructors whose DData is
never produced by normal @DType@ translation.

Constructors are listed in lexicographic order because
'CaseExpressionCanonicalization' sorts 'ECase' clauses lexicographically
and the LLVM codegen assigns switch tags by clause position.
-}
builtinDData :: [Kernel.Object Kernel.Type]
builtinDData =
  -- List: $Cons (tag 0) < $Nil (tag 1) lexicographically
  Kernel.DData
    "list"
    [ ("$Cons", Kernel.arrow Kernel.TOpq (Kernel.arrow (Kernel.TCon "list" [Kernel.TOpq]) (Kernel.TCon "list" [Kernel.TOpq])))
    , ("$Nil", Kernel.TCon "list" [Kernel.TOpq])
    ]
    :
    -- Record
    Kernel.DData
      "record"
      [("$Record", Kernel.arrow Kernel.TOpq (Kernel.TCon "record" [Kernel.TOpq]))]
    :
    -- Nat: $Succ (tag 0) < $Zero (tag 1) lexicographically
    Kernel.DData
      "nat"
      [ ("$Succ", Kernel.arrow (Kernel.TCon "int32" []) (Kernel.TCon "nat" []))
      , ("$Zero", Kernel.TCon "nat" [])
      ]
    :
    -- Tuples $Tuple2 .. $Tuple8 (each type has one constructor at tag 0)
    [ Kernel.DData
      ("tuple" <> showt n)
      [("$Tuple" <> showt n, foldr Kernel.arrow (Kernel.TCon "tuple" (replicate n Kernel.TOpq)) (replicate n Kernel.TOpq))]
    | n <- [2 .. 8 :: Int]
    ]
