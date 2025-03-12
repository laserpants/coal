{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Noll.Core.Compiler.Pipeline (
  Pipeline (..),
  runPipeline,
  evalPipeline,
  extendInterpreterValueEnv,
  extendInterpreterConstructorEnv,
  pipelineInsertArtifacts,
  pipelineInsertCode,
) where

import Control.Monad.State (MonadState, State, evalState, modify, runState)
import Noll.Common.Environment (Environment)
import Noll.Core.Compiler.Kernel (Kernel (..), initialKernel, overKernelArtifacts, overKernelCode, overKernelInterpreterConstructorEnv, overKernelInterpreterValueEnv, overKernelSupply)
import Noll.Core.LLVM.IRConstruct (IRConstruct (..))
import Noll.Core.LLVM.IRInterpreter.Artifact (IRInterpreterArtifact (..))
import Noll.Core.LLVM.IRInterpreter.Monad (IRLine (..))
import Noll.Core.LLVM.IRValue (IRValue (..))

newtype Pipeline a = Pipeline {pipelineKernel :: State Kernel a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState Kernel
    )

runPipeline :: Pipeline a -> (a, Kernel)
runPipeline p = runState (pipelineKernel p) initialKernel

evalPipeline :: Pipeline a -> a
evalPipeline p = evalState (pipelineKernel p) initialKernel

{-# INLINE extendInterpreterValueEnv #-}
extendInterpreterValueEnv :: Environment IRValue -> Pipeline ()
extendInterpreterValueEnv env = modify (overKernelInterpreterValueEnv (<> env))

{-# INLINE extendInterpreterConstructorEnv #-}
extendInterpreterConstructorEnv :: Environment Int -> Pipeline ()
extendInterpreterConstructorEnv env = modify (overKernelInterpreterConstructorEnv (<> env))

{-# INLINE pipelineInsertArtifacts #-}
pipelineInsertArtifacts :: [IRInterpreterArtifact] -> Pipeline ()
pipelineInsertArtifacts = modify . (overKernelArtifacts . (<>))

{-# INLINE pipelineInsertCode #-}
pipelineInsertCode :: [IRConstruct [IRLine]] -> Pipeline ()
pipelineInsertCode = modify . (overKernelCode . (<>))
