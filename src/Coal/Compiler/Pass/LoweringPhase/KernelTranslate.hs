{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.LoweringPhase.KernelTranslate (passKernelTranslate) where

import Coal.AST.Metadata (Metadata (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build (Build (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Kernel.Environment (insertQualifiedNames, withModuleName)
import Coal.Compiler.Kernel.Translate.Definition (translateDefinition)
import Coal.Compiler.Pass (Pass (..), tickBar)
import Coal.Compiler.Stack
import qualified Coal.Kernel.Language as Kernel
import Coal.Language (IndexedType, Kind (..))
import Coal.Language.Module (Module (Module))
import Coal.Language.Module.Path (principalPath)
import Control.Monad.IO.Class (MonadIO)
import Extras (Name)

passKernelTranslate :: (MonadIO m) => Pass Metadata m (BuildEnvelope (Module Metadata Kind IndexedType)) (BuildEnvelope (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)))
passKernelTranslate = Pass{runPass = \p -> tickBar >> traverse pass p}

pass :: (MonadIO m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))
pass =
  \case
    Module path _ defs -> do
      setCurrentPathC path
      Build{..} <- getCurrentBuildC
      insertQualifiedNames buildQualifiedNames $
        withModuleName name $
          Kernel.Module
            name
            (Environment.elems buildQualifiedNames)
            . concat
            <$> traverse translateDefinition defs
     where
      name = principalPath path
