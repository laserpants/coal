{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.LoweringPhase.KernelTranslate (passKernelTranslate) where

import Coal.AST.Metadata (Metadata (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build.Unit (BuildUnit (..))
import Coal.Compiler.Kernel.Environment (insertQualifiedNames, withModuleName)
import Coal.Compiler.Kernel.Translate.Definition (translateDefinition)
import Coal.Compiler.Pass (Pass (..), tickBar)
import Coal.Compiler.Stack
import qualified Coal.Kernel.Language as Kernel
import Coal.Language (IndexedType, Kind (..))
import Coal.Language.Module (Module (..), principalPath)
import Coal.ProtoCompiler.ProtoBuild (ProtoBuild (..))
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT (..), protoOgetCurrentBuildC, setCurrentPathC)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import Extras (Name)

passKernelTranslate :: (MonadIO m) => Pass Metadata m (BuildUnit (Module Metadata Kind IndexedType)) (BuildUnit (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)))
passKernelTranslate = Pass{runPass = \p -> tickBar >> traverse pass p}

pass :: (MonadIO m) => Module Metadata Kind IndexedType -> CompilerT Metadata (ProtoCompilerT m Metadata) (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))
pass =
  \case
    Module path _ defs -> do
      lift $ setCurrentPathC path
      ProtoBuild{..} <- lift protoOgetCurrentBuildC
      insertQualifiedNames protoObuildQualifiedNames $
        withModuleName name $
          Kernel.Module
            name
            (Environment.elems protoObuildQualifiedNames)
            . concat
            <$> traverse translateDefinition defs
     where
      name = principalPath path
