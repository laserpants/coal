{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Compiler.Utils (
  testRunner,
  testModules,
  runTest,
  unsafeParseExpr,
) where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Coal.Kernel.Compiler
import Coal.Kernel.LLVM.IRConstruct (IRConstruct (..))
import Coal.Kernel.LLVM.IREncodable (IREncodable (..))
import Coal.Kernel.LLVM.IRInterpreter.Monad (IRLine)
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Parser (spaces)
import Coal.Kernel.Parser.Expr (expr)
import Coal.Kernel.Parser.Module (module_)
import Extra (Name, forM_)
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Text.Megaparsec (eof, runParser)
import Text.Megaparsec.Error (errorBundlePretty)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Coal.Kernel.Language as Core

unsafeParseExpr :: Text -> Core.Expr Core.Type
unsafeParseExpr t =
  case runParser expr "" (Text.stripStart t) of
    Left e ->
      error (errorBundlePretty e)
    Right r ->
      r

unsafeParseModule :: Text -> Module Core.Type Name (Core.Expr Core.Type)
unsafeParseModule t =
  case runParser (spaces *> module_ <* eof) "" t of
    Left e ->
      error (errorBundlePretty e)
    Right r ->
      r

unsafeParseFile :: Text -> Module Core.Type Name (Core.Expr Core.Type)
unsafeParseFile path = unsafeParseModule (unsafePerformIO (Text.readFile (Text.unpack path)))

--getName :: Text -> Text
--getName path = fromMaybe name (Text.stripSuffix ".txt" name)
-- where
--  name = last parts
--  parts = Text.splitOn "/" path

buildScript :: [Text] -> Text
buildScript modules =
  Text.unlines
    ( [ "#!/bin/bash"
      , "cd \"$(dirname \"$0\")\" || exit 1"
      ]
        <> [ llcCmd name | name <- modules
           ]
        <> [ "gcc -g -I./ -lgc -lgmp ../runtime/lib.c " <> Text.concat [name <> ".o " | name <- modules] <> "-o dist"
           ]
    )
 where
  llcCmd name =
    "llc -filetype=obj "
      <> name
      <> ".ll -o "
      <> name
      <> ".o"

testModules :: [(Name, [IRConstruct [IRLine]])] -> IO ()
testModules mods = do
  forM_ mods $
    \(name, code) -> do
      let out = irEncode code
      Text.writeFile ("./.build/" <> Text.unpack name <> ".ll") out
  Text.writeFile "./.build/build.sh" (buildScript (fst <$> mods))

testRunner :: [Text] -> IO ()
testRunner input = testModules =<< compileModules (unsafeParseFile <$> input)

runTest :: IO () -> IO Text
runTest test = do
  test
  void $ readProcess "./.build/build.sh" [] ""
  Text.pack <$> readProcess "./.build/dist" [] ""
