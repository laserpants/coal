{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coal.Kernel.Compiler.Pipeline (
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

import Coal.Common.Environment (Environment)
import Coal.Kernel.Compiler.Pipeline.State
import Coal.Kernel.LLVM
import Coal.Kernel.Language (Type)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, StateT, evalStateT, modify, runStateT)
import Data.List (sort)
import Extras (Name)

import qualified Coal.Common.Environment as Environment

newtype Pipeline a = Pipeline {pipelineState :: StateT PipelineState IO a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState PipelineState
    )

{-# INLINE runPipeline #-}
runPipeline :: Pipeline a -> IO (a, PipelineState)
runPipeline p = runStateT (pipelineState p) initialPipelineState

{-# INLINE evalPipeline #-}
evalPipeline :: Pipeline a -> IO a
evalPipeline p = evalStateT (pipelineState p) initialPipelineState

{-# INLINE extendInterpreterValueEnv #-}
extendInterpreterValueEnv :: Environment IRValue -> Pipeline ()
extendInterpreterValueEnv env = modify (overPipelineStateInterpreterValueEnv (<> env))

{-# INLINE extendInterpreterConstructorEnv #-}
extendInterpreterConstructorEnv :: Environment Int -> Pipeline ()
extendInterpreterConstructorEnv env = modify (overPipelineStateInterpreterConstructorEnv (<> env))

{-# INLINE pipelineInsertArtifacts #-}
pipelineInsertArtifacts :: [IRInterpreterArtifact] -> Pipeline ()
pipelineInsertArtifacts = modify . (overPipelineStateArtifacts . (<>))

{-# INLINE pipelineInsertCode #-}
pipelineInsertCode :: [IRConstruct [IRLine]] -> Pipeline ()
pipelineInsertCode code = modify (overPipelineStateCode (sort . (<> code)))

{-# INLINE pipelineInsertNames #-}
pipelineInsertNames :: [(Name, Type)] -> Pipeline ()
pipelineInsertNames names = modify (overPipelineStateNames (Environment.insertMultiple names))

{-# INLINE pipelineInsertIRTypes #-}
pipelineInsertIRTypes :: [(Name, IRType)] -> Pipeline ()
pipelineInsertIRTypes names = modify (overPipelineStateIRTypes (Environment.insertMultiple names))

{-# INLINE pipelineInsertConstructors #-}
pipelineInsertConstructors :: [(Name, Int)] -> Pipeline ()
pipelineInsertConstructors ctors = modify (overPipelineStateConstructors (Environment.insertMultiple ctors))

{-# INLINE pipelineReset #-}
pipelineReset :: Pipeline ()
pipelineReset = modify resetPipelineState

{-# INLINE pipelineResetSupply #-}
pipelineResetSupply :: Pipeline ()
pipelineResetSupply = modify (overPipelineStateSupply (const 0))
