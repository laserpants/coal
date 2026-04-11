{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.LoweringPhase.KernelTranslate (passKernelTranslate) where

import Coal.AST.Metadata (Metadata (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Kernel.Environment (insertQualifiedNames, withModuleName)
import Coal.Compiler.Kernel.Translate.Definition (translateDefinition)
import Coal.Compiler.Pass (Pass (..), tickBar)
import Coal.Compiler.ProtoBuild (ProtoBuild (..))
import Coal.Compiler.Stack
import qualified Coal.Kernel.Language as Kernel
import Coal.Language (IndexedType, Kind (..))
import Coal.Language.Module.Path
import Coal.ProtoLanguage.ProtoModule
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import Extras (Name)

passKernelTranslate :: (MonadIO m) => Pass Metadata m (BuildEnvelope (ProtoModule Metadata Kind IndexedType)) (BuildEnvelope (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)))
passKernelTranslate = Pass{runPass = \p -> tickBar >> traverse pass p}

pass :: (MonadIO m) => ProtoModule Metadata Kind IndexedType -> CompilerT Metadata m (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))
pass =
  \case
    ProtoModule path _ defs -> do
      setCurrentPathC path
      ProtoBuild{..} <- protoOgetCurrentBuildC
      insertQualifiedNames protoObuildQualifiedNames $
        withModuleName name $
          Kernel.Module
            name
            (Environment.elems protoObuildQualifiedNames)
            . concat
            <$> traverse translateDefinition defs
     where
      name = principalPath path

-- pass :: (MonadIO m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))
-- pass =
--  \case
--    Module path _ defs -> do
--      lift $ setCurrentPathC path
--      ProtoBuild{..} <- lift protoOgetCurrentBuildC
--      insertQualifiedNames protoObuildQualifiedNames $
--        withModuleName name $
--          Kernel.Module
--            name
--            (Environment.elems protoObuildQualifiedNames)
--            . concat
--            <$> traverse translateDefinition defs
--     where
--      name = principalPath path
