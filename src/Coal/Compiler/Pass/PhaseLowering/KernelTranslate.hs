{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Coal.Compiler.Pass.PhaseLowering.KernelTranslate (passKernelTranslate) where

import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build (Build (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Kernel.Environment (insertQualifiedNames, withModuleName)
import Coal.Compiler.Kernel.Translate.Definition (translateDefinition)
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), tickBar)
import Coal.Compiler.Stack (CompilerT, getCurrentBuildC, setCurrentPathC)
import qualified Coal.Kernel.Language as Kernel
import Coal.Language (IndexedType, Kind (..))
import Coal.Language.Module (Module (Module))
import Coal.Language.Module.Path (principalPath)
import Control.Monad.IO.Class (MonadIO)
import Extras (Name, concatForM)

passKernelTranslate :: (MonadIO m) => Pass Metadata m (BuildEnvelope (Module Metadata Kind IndexedType)) (BuildEnvelope (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)))
passKernelTranslate = Pass{runPass = passImpl}

passImpl :: (MonadIO m, Traversable t) => t (Module Metadata Kind IndexedType) -> CompilerT Metadata m (t (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)))
passImpl p = tickBar >> traverse pass p

pass :: (MonadIO m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))
pass =
  \case
    Module path _ defs -> do
      setCurrentPathC path
      Build{buildQualifiedNames} <- getCurrentBuildC
      insertQualifiedNames buildQualifiedNames $
        let
          moduleName = principalPath path
         in
          withModuleName moduleName $ do
            newObjects <- concatForM defs translateDefinition
            return $
              Kernel.Module
                { moduleName = moduleName
                , moduleImports = Environment.elems buildQualifiedNames
                , moduleObjects = newObjects
                }
