{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.LoweringPhase.KernelTranslate (passKernelTranslate) where

import Coal.AST.Metadata (Metadata (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build
import Coal.Compiler.Kernel.Environment (insertQualifiedNames, withModuleName)
import Coal.Compiler.Kernel.Translate.Definition (translateDefinition)
import Coal.Compiler.Pass (Pass (..), tickBar)
import Coal.Compiler.Stack
import qualified Coal.Kernel.Language as Kernel
import Coal.Language (IndexedType, Kind (..))
import Coal.Language.Module
import Control.Monad.IO.Class (MonadIO)
import Extras (Name)

passKernelTranslate :: (MonadIO m) => Pass Metadata m (Module Metadata Kind IndexedType) (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))
passKernelTranslate = Pass{runPass = pass}

pass :: (MonadIO m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))
pass =
  \case
    Module path _ defs -> do
      tickBar

      setCompilerCurrentModuleC path
      ModuleBuild{..} <- getCurrentBuildC

      insertQualifiedNames moduleQualifiedNames $
        withModuleName name $
          Kernel.Module
            name
            (Environment.elems moduleQualifiedNames)
            . concat
            <$> traverse translateDefinition defs
     where
      name = principalPath path
