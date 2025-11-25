{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.LoweringPhase.KernelCode (passKernelCode) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Kernel.Builtin.Objects (builtinObjects)
import qualified Coal.Kernel.Compiler as Kernel
import Coal.Kernel.LLVM (IRConstruct, IRLine)
import qualified Coal.Kernel.Language as Kernel
import Control.Monad.IO.Class (MonadIO (..))
import Extras (Name)

passKernelCode :: (MonadIO m) => Pass Metadata m [Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)] [(Name, [IRConstruct [IRLine]])]
passKernelCode =
  Pass
    { passName = "KernelCode"
    , runPass = pass
    }

pass :: (MonadIO m) => [Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)] -> CompilerT Metadata m [(Name, [IRConstruct [IRLine]])]
pass ms = liftIO $ Kernel.compileModules (builtinObjects : ms)
