{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.PhaseLowering.KernelCode (
  passKernelCode,
  compileEnvelopes,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT, updateBuildC)
import Coal.Kernel.Builtin.Objects (builtinObjects)
import Coal.Kernel.Compiler (KernelExpr, compile, compileClosureCode)
import Coal.Kernel.Compiler.EntryPoint (entryPoint)
import Coal.Kernel.Compiler.Pass (transformInterpreter)
import Coal.Kernel.Compiler.Pipeline
import Coal.Kernel.Compiler.Pipeline.State (PipelineState (..))
import Coal.Kernel.LLVM
import Coal.Kernel.Language
import qualified Coal.Kernel.Language as Kernel
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (gets)
import Control.Monad.Trans (lift)
import qualified Data.Text as Text
import Extras (Name, forM, isConstructor, (<$$>))

passKernelCode :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))] [BuildEnvelope (Name, [IRConstruct [IRLine]])]
passKernelCode = Pass{runPass = passImpl}

passImpl :: (MonadIO m) => [BuildEnvelope (Module Type Name (Expr Type))] -> CompilerT Metadata m [BuildEnvelope (Name, [IRConstruct [IRLine]])]
passImpl modules = evalPipelineT (compileEnvelopes (builtin : modules))

compileEnvelopes :: (MonadIO m) => [BuildEnvelope (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))] -> PipelineT (CompilerT Metadata m) [BuildEnvelope (Name, [IRConstruct [IRLine]])]
compileEnvelopes units = do
  mods <- forM units $
    \case
      BSource Module{moduleName, moduleImports, moduleObjects} -> do
        pipelineReset

        names <- collectNames moduleImports
        ctors <- collectConstructors moduleImports

        ir <- compileModule ctors Module{moduleImports = names, ..}
        IRInterpreterEnv{..} <- gets pipelineInterpreterEnv

        let kernelNames = objectSignature <$> moduleObjects
            irTypes = irTypeOf <$$> Environment.toList irInterpreterValueEnv
            kernelConstructors = objectConstructorInfo =<< moduleObjects

        pipelineInsertNames kernelNames
        pipelineInsertIRTypes irTypes
        pipelineInsertConstructors kernelConstructors

        let out = ir <> [entryPoint | moduleName == "Main"]

        lift $
          unless (moduleName == "Builtin$") $ do
            updateBuildC moduleName $
              \Build{..} ->
                pure
                  Build
                    { buildKernelNames = Environment.fromList kernelNames
                    , buildKernelIRTypes = Environment.fromList irTypes
                    , buildKernelConstructors = Environment.fromList kernelConstructors
                    , ..
                    }

        pure (BSource (moduleName, out))
      BCached Build{..} -> do
        pipelineInsertNames (Environment.toList buildKernelNames)
        pipelineInsertIRTypes (Environment.toList buildKernelIRTypes)
        pipelineInsertConstructors (Environment.toList buildKernelConstructors)
        pure (BCached Build{..})

  cc <- transformInterpreter compileClosureCode
  -- TODO: cache?
  pure (BSource ("closures", cc) : mods)
 where
  collectNames :: (MonadIO m) => [Name] -> PipelineT m [(Name, Type)]
  collectNames names = gets (Environment.lookupAll names . Environment.filterNames notConstructor . pipelineNames)

  notConstructor :: Name -> Bool
  notConstructor name =
    let parts = Text.splitOn "." name
     in not (isConstructor (last parts))

  collectConstructors :: (MonadIO m) => [Name] -> PipelineT m [(Name, Int)]
  collectConstructors names = gets (Environment.lookupAll names . pipelineConstructors)

  objectSignature :: Object Type KernelExpr -> (Name, Type)
  objectSignature obj = (objectName obj, typeOf obj)

  compileModule :: (MonadIO m) => [(Name, Int)] -> Module Type (Name, Type) KernelExpr -> PipelineT m [IRConstruct [IRLine]]
  compileModule ctors Module{..} = do
    names <- gets pipelineIRTypes
    let external = toExternal names <$> moduleImports
    compile (Environment.fromList ctors) (external <> moduleObjects)
    gets pipelineCode

  toExternal :: Environment IRType -> (Name, Type) -> Object Type KernelExpr
  toExternal env (name, t) =
    case Environment.lookup name env of
      Nothing ->
        error ("Name not in scope: '" <> Text.unpack name <> "'")
      Just it ->
        OExternal name it t

-- TODO: cache?
builtin :: BuildEnvelope (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))
builtin = BSource builtinObjects
