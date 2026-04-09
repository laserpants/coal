{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.LoweringPhase.KernelCode (passKernelCode, compileUnits) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build.Unit (BuildUnit (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Kernel.Builtin.Objects (builtinObjects)
import Coal.Kernel.Compiler (KernelExpr, compile, compileClosureCode)
import Coal.Kernel.Compiler.EntryPoint (entryPoint)
import Coal.Kernel.Compiler.Pass (transformInterpreter)
import Coal.Kernel.Compiler.Pipeline
import Coal.Kernel.Compiler.Pipeline.State (PipelineState (..))
import Coal.Kernel.LLVM
import Coal.Kernel.Language
import qualified Coal.Kernel.Language as Kernel
import Coal.ProtoCompiler.ProtoBuild
import Coal.ProtoCompiler.ProtoStack
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (gets)
import Control.Monad.Trans (lift)
import qualified Data.Text as Text
import Extras (Name, forM, isConstructor, (<$$>))

passKernelCode :: (MonadIO m) => Pass Metadata m [BuildUnit (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))] [BuildUnit (Name, [IRConstruct [IRLine]])]
passKernelCode = Pass{runPass = evalPipelineT . pass}

pass :: (MonadIO m) => [BuildUnit (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))] -> PipelineT (CompilerT Metadata (ProtoCompilerT m a)) [BuildUnit (Name, [IRConstruct [IRLine]])]
pass ms = compileUnits (builtin : ms)

compileUnits :: (MonadIO m) => [BuildUnit (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))] -> PipelineT (CompilerT Metadata (ProtoCompilerT m a)) [BuildUnit (Name, [IRConstruct [IRLine]])]
compileUnits units = do
  mods <- forM units $
    \case
      BSource Module{..} -> do
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
            lift $
              protoOupdateBuildC moduleName $
                \ProtoBuild{..} ->
                  pure
                    ProtoBuild
                      { protoObuildKernelNames = Environment.fromList kernelNames
                      , protoObuildKernelIRTypes = Environment.fromList irTypes
                      , protoObuildKernelConstructors = Environment.fromList kernelConstructors
                      , ..
                      }

        pure (BSource (moduleName, out))
      BCached ProtoBuild{..} -> do
        pipelineInsertNames (Environment.toList protoObuildKernelNames)
        pipelineInsertIRTypes (Environment.toList protoObuildKernelIRTypes)
        pipelineInsertConstructors (Environment.toList protoObuildKernelConstructors)
        pure (BCached ProtoBuild{..})

  cc <- transformInterpreter compileClosureCode
  -- TODO: cache
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

-- TODO: cache
builtin :: BuildUnit (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))
builtin = BSource builtinObjects
