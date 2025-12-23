{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coal.Kernel.Compiler.Pipeline (
  PipelineT (..),
  runPipelineT,
  evalPipelineT,
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
import qualified Coal.Common.Environment as Environment
import Coal.Kernel.Compiler.Pipeline.State
import Coal.Kernel.LLVM
import Coal.Kernel.Language (Type)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, MonadTrans, StateT, evalStateT, modify, runStateT)
import Data.List (sort)
import Extras (Name)

newtype PipelineT m a = PipelineT {pipelineStateT :: StateT PipelineState m a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState PipelineState
    , MonadIO
    , MonadTrans
    )

{-# INLINE runPipelineT #-}
runPipelineT :: PipelineT m a -> m (a, PipelineState)
runPipelineT p = runStateT (pipelineStateT p) initialPipelineState

{-# INLINE evalPipelineT #-}
evalPipelineT :: (Monad m) => PipelineT m a -> m a
evalPipelineT p = evalStateT (pipelineStateT p) initialPipelineState

{-# INLINE extendInterpreterValueEnv #-}
extendInterpreterValueEnv :: (Monad m) => Environment IRValue -> PipelineT m ()
extendInterpreterValueEnv env = modify (overPipelineStateInterpreterValueEnv (<> env))

{-# INLINE extendInterpreterConstructorEnv #-}
extendInterpreterConstructorEnv :: (Monad m) => Environment Int -> PipelineT m ()
extendInterpreterConstructorEnv env = modify (overPipelineStateInterpreterConstructorEnv (<> env))

{-# INLINE pipelineInsertArtifacts #-}
pipelineInsertArtifacts :: (Monad m) => [IRInterpreterArtifact] -> PipelineT m ()
pipelineInsertArtifacts = modify . (overPipelineStateArtifacts . (<>))

{-# INLINE pipelineInsertCode #-}
pipelineInsertCode :: (Monad m) => [IRConstruct [IRLine]] -> PipelineT m ()
pipelineInsertCode code = modify (overPipelineStateCode (sort . (<> code)))

{-# INLINE pipelineInsertNames #-}
pipelineInsertNames :: (Monad m) => [(Name, Type)] -> PipelineT m ()
pipelineInsertNames names = modify (overPipelineStateNames (Environment.insertMultiple names))

{-# INLINE pipelineInsertIRTypes #-}
pipelineInsertIRTypes :: (Monad m) => [(Name, IRType)] -> PipelineT m ()
pipelineInsertIRTypes names = modify (overPipelineStateIRTypes (Environment.insertMultiple names))

{-# INLINE pipelineInsertConstructors #-}
pipelineInsertConstructors :: (Monad m) => [(Name, Int)] -> PipelineT m ()
pipelineInsertConstructors ctors = modify (overPipelineStateConstructors (Environment.insertMultiple ctors))

{-# INLINE pipelineReset #-}
pipelineReset :: (Monad m) => PipelineT m ()
pipelineReset = modify resetPipelineState

{-# INLINE pipelineResetSupply #-}
pipelineResetSupply :: (Monad m) => PipelineT m ()
pipelineResetSupply = modify (overPipelineStateSupply (const 0))
