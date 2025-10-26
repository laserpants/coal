{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.LoweringPhase.KernelCode (passKernelCode) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Pass
import Coal.Compiler.Stack
import Coal.Kernel.Builtin.Objects (builtinObjects)
import qualified Coal.Kernel.Compiler as Kernel
import Coal.Kernel.LLVM
import qualified Coal.Kernel.Language as Kernel
import Control.Monad.IO.Class
import Extras (Name)

passKernelCode :: (MonadIO m) => Pass Metadata m [Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)] [(Name, [IRConstruct [IRLine]])]
passKernelCode =
  Pass
    { passName = "KernelCode"
    , runPass = pass
    }

pass :: (MonadIO m) => [Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)] -> CompilerT Metadata m [(Name, [IRConstruct [IRLine]])]
pass ms = liftIO $ Kernel.compileModules (builtinObjects : ms)
