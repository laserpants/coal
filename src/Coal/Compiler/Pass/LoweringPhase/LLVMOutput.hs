{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.LoweringPhase.LLVMOutput (passLLVMOutput) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Pass
import Coal.Compiler.Stack
import Coal.Kernel.LLVM.IRConstruct (IRConstruct (..))
import Coal.Kernel.LLVM.IREncodable (irEncode)
import Coal.Kernel.LLVM.IRInterpreter.Monad
import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Extra (Name)

passLLVMOutput :: (MonadIO m) => Pass Metadata m [(Name, [IRConstruct [IRLine]])] ()
passLLVMOutput =
  Pass
    { passName = "LLVMOutput"
    , runPass = pass
    }

pass :: (MonadIO m) => [(Name, [IRConstruct [IRLine]])] -> CompilerT Metadata m ()
pass = liftIO . generateLLOutput

generateLLOutput :: [(Name, [IRConstruct [IRLine]])] -> IO ()
generateLLOutput mods = do
  forM_ mods $
    \(name, code) -> do
      let out = irEncode code
      Text.writeFile ("./.build/" <> Text.unpack name <> ".ll") out
  Text.writeFile "./.build/build.sh" (buildScript (fst <$> mods))

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
