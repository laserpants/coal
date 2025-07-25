{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Noll.Kernel.Compiler.Pipeline (
  Pipeline (..),
  runPipeline,
  evalPipeline,
  extendInterpreterValueEnv,
  extendInterpreterConstructorEnv,
  pipelineInsertArtifacts,
  pipelineInsertCode,
  pipelineInsertNames,
  pipelineInsertIRTypes,
  pipelineInsertConstructors,
  pipelineReset,
  pipelineResetSupply,
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, StateT, evalStateT, modify, runStateT)
import Data.List (sort)
import Lang.Common.Environment (Environment)
import Noll.Kernel.Compiler.Pipeline.Kernel
import Noll.Kernel.LLVM
import Noll.Kernel.Language (Type)
import Extra (Name)

import qualified Lang.Common.Environment as Environment

newtype Pipeline a = Pipeline {pipelineKernel :: StateT Kernel IO a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState Kernel
    )

{-# INLINE runPipeline #-}
runPipeline :: Pipeline a -> IO (a, Kernel)
runPipeline p = runStateT (pipelineKernel p) initialKernel

{-# INLINE evalPipeline #-}
evalPipeline :: Pipeline a -> IO a
evalPipeline p = evalStateT (pipelineKernel p) initialKernel

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
pipelineInsertCode code = modify (overKernelCode (sort . (<> code)))

{-# INLINE pipelineInsertNames #-}
pipelineInsertNames :: [(Name, Type)] -> Pipeline ()
pipelineInsertNames names = modify (overKernelNames (Environment.insertMultiple names))

{-# INLINE pipelineInsertIRTypes #-}
pipelineInsertIRTypes :: [(Name, IRType)] -> Pipeline ()
pipelineInsertIRTypes names = modify (overKernelIRTypes (Environment.insertMultiple names))

{-# INLINE pipelineInsertConstructors #-}
pipelineInsertConstructors :: [(Name, Int)] -> Pipeline ()
pipelineInsertConstructors ctors = modify (overKernelConstructors (Environment.insertMultiple ctors))

{-# INLINE pipelineReset #-}
pipelineReset :: Pipeline ()
pipelineReset = modify resetKernel

{-# INLINE pipelineResetSupply #-}
pipelineResetSupply :: Pipeline ()
pipelineResetSupply = modify (overKernelSupply (const 0))
