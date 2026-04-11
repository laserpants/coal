{-# LANGUAGE OverloadedStrings #-}

module E2E.Kernel.Spec (e2eKernelSpec) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Name (Name)
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Config (CompilerConfig (..), defaultConfig)
import Coal.Compiler.Environment (emptyCompilerEnvironment)
import Coal.Compiler.Pass.LoweringPhase.KernelCode (compileUnits)
import Coal.Compiler.Pass.LoweringPhase.LLVMOutput (generateLLOutput)
import Coal.Compiler.Pass.LoweringPhase.Linking (compileBitcode)
import Coal.Compiler.Stack
import Coal.Kernel.Builtin.Objects (builtinObjects)
import Coal.Kernel.Compiler.Pipeline (evalPipelineT)
import qualified Coal.Kernel.Language as Kernel
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Parser (spaces)
import Coal.Kernel.Parser.Module (module_)
import Coal.ProtoCompiler.ProtoStack
import Control.Monad (void)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Test.Hspec
import Text.Megaparsec (eof, runParser)
import Text.Megaparsec.Error (errorBundlePretty)

e2eKernelSpec :: Spec
e2eKernelSpec = do
  describe "001" $ do
    expectOutput
      "1"
      [ "./test/Coal/Kernel/examples/001/Data.List.txt"
      , "./test/Coal/Kernel/examples/001/My.Utilities.txt"
      , "./test/Coal/Kernel/examples/001/Main.txt"
      ]

  describe "002" $ do
    expectOutput
      "103"
      [ "./test/Coal/Kernel/examples/002/Utils.txt"
      , "./test/Coal/Kernel/examples/002/Utils.Function.txt"
      , "./test/Coal/Kernel/examples/002/Ordering.txt"
      , "./test/Coal/Kernel/examples/002/List.txt"
      , "./test/Coal/Kernel/examples/002/Tree.txt"
      , "./test/Coal/Kernel/examples/002/Main.txt"
      ]

  describe "003" $ do
    expectOutput
      "3"
      [ "./test/Coal/Kernel/examples/003/Main.txt"
      ]

  describe "004" $ do
    expectOutput
      "3"
      [ "./test/Coal/Kernel/examples/004/Data.Tree.txt"
      , "./test/Coal/Kernel/examples/004/Main.txt"
      ]

  describe "005" $ do
    expectOutput
      "6"
      [ "./test/Coal/Kernel/examples/005/Main.txt"
      ]

  describe "006" $ do
    expectOutput
      "6"
      [ "./test/Coal/Kernel/examples/006/Main.txt"
      ]

  describe "007" $ do
    expectOutput
      "Hello, world! 🚀"
      [ "./test/Coal/Kernel/examples/007/Main.txt"
      ]

  describe "008" $ do
    expectOutput
      "🤖🤖 Hello, world!"
      [ "./test/Coal/Kernel/examples/008/Main.txt"
      ]

  describe "009" $ do
    expectOutput
      "🎷"
      [ "./test/Coal/Kernel/examples/009/Main.txt"
      ]

  describe "010" $ do
    expectOutput
      "4.500000\n4"
      [ "./test/Coal/Kernel/examples/010/Main.txt"
      ]

  describe "011" $ do
    expectOutput
      "10"
      [ "./test/Coal/Kernel/examples/011/Main.txt"
      ]

  describe "012" $ do
    expectOutput
      "133"
      [ "./test/Coal/Kernel/examples/012/Group.txt"
      , "./test/Coal/Kernel/examples/012/Main.txt"
      ]

  describe "013" $ do
    expectOutput
      "3"
      [ "./test/Coal/Kernel/examples/013/Main.txt"
      ]

  describe "014" $ do
    expectOutput
      "9999999999999999999999999999999999999998"
      [ "./test/Coal/Kernel/examples/014/Main.txt"
      ]

  describe "015" $ do
    expectOutput
      "1"
      [ "./test/Coal/Kernel/examples/015/Core$.txt"
      , "./test/Coal/Kernel/examples/015/Ordered.txt"
      , "./test/Coal/Kernel/examples/015/BinarySearch.txt"
      , "./test/Coal/Kernel/examples/015/Main.txt"
      ]

  describe "016" $ do
    expectOutput
      "4"
      [ "./test/Coal/Kernel/examples/016/Main.txt"
      ]

  describe "017" $ do
    expectOutput
      "5"
      [ "./test/Coal/Kernel/examples/017/Core$.txt"
      , "./test/Coal/Kernel/examples/017/Main.txt"
      ]

  describe "018" $ do
    expectOutput
      "5"
      [ "./test/Coal/Kernel/examples/018/Core$.txt"
      , "./test/Coal/Kernel/examples/018/Main.txt"
      ]

expectOutput :: String -> [FilePath] -> Spec
expectOutput expt files =
  it ("\"" <> expt <> "\"") $ do
    res <- evalProtoCompilerT (evalCompilerT (emptyCompilerEnvironment Nothing) (runKernelSpec files))
    res `shouldBe` Right (Right expt)

runKernelSpec :: [FilePath] -> CompilerT Metadata (ProtoCompilerT IO a) String -- (Either CompilerFailureMode String)
runKernelSpec files = do
  ir <- evalPipelineT (compileUnits (BSource builtinObjects : mods))
  res <- generateLLOutput Nothing config ir
  case res of
    Left err ->
      throwError err
    Right bc -> do
      liftIO $ do
        void $ compileBitcode config bc
        readProcess "./dist" [] ""
 where
  mods = BSource . unsafeParseFile <$> files
  config =
    defaultConfig
      { configGenerateDotFiles = False
      , configGenerateLLVMOutput = False
      }

unsafeParseFile :: FilePath -> Module Kernel.Type Name (Kernel.Expr Kernel.Type)
unsafeParseFile path = unsafeParseModule (unsafePerformIO (Text.readFile path))

unsafeParseModule :: Text -> Module Kernel.Type Name (Kernel.Expr Kernel.Type)
unsafeParseModule t =
  case runParser (spaces *> module_ <* eof) "" t of
    Left e ->
      error (errorBundlePretty e)
    Right r ->
      r
