{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

{- | New kernel translation pass.

Translates surface language modules to new kernel language modules using
'Coal.Compiler.Kernel.Translate.Definition.translateDefinition'.
This pass runs in parallel with 'passKernelTranslate' (the legacy pass)
during the migration period.
-}
module Coal.Compiler.Pass.PhaseLowering.KernelTranslate (passKernelTranslate) where

import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build (Build (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Kernel.Environment (insertQualifiedNames, withModuleName)
import Coal.Compiler.Kernel.Translate.Definition (translateDefinition)
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT, getCurrentBuildC, setCurrentPathC)
import qualified Coal.Kernel.Language.Module as NKModule (Module (..))
import qualified Coal.Kernel.Language.Type as NK
import Coal.Language (IndexedType, Kind (..))
import qualified Coal.Language.Module as SurfModule (Module (..))
import Coal.Language.Module.Path (principalPath)
import Control.Monad.IO.Class (MonadIO)
import Extras (concatForM)

passKernelTranslate ::
  (MonadIO m) =>
  Pass
    Metadata
    m
    (BuildEnvelope (SurfModule.Module Metadata Kind IndexedType))
    (BuildEnvelope (NKModule.Module NK.Type))
passKernelTranslate = Pass{runPass = passImpl}

passImpl ::
  (MonadIO m, Traversable t) =>
  t (SurfModule.Module Metadata Kind IndexedType) ->
  CompilerT Metadata m (t (NKModule.Module NK.Type))
passImpl = traverse pass

pass ::
  (MonadIO m) =>
  SurfModule.Module Metadata Kind IndexedType ->
  CompilerT Metadata m (NKModule.Module NK.Type)
pass =
  \case
    SurfModule.Module path _ defs -> do
      setCurrentPathC path
      Build{buildQualifiedNames} <- getCurrentBuildC
      insertQualifiedNames buildQualifiedNames $
        let moduleName = principalPath path
         in withModuleName moduleName $ do
              newObjects <- concatForM defs translateDefinition
              return
                NKModule.Module
                  { NKModule.moduleName = moduleName
                  , NKModule.moduleImports = Environment.elems buildQualifiedNames
                  , NKModule.moduleObjects = newObjects
                  }
