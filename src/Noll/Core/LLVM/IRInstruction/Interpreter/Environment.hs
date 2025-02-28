{-# LANGUAGE RecordWildCards #-}

module Noll.Core.LLVM.IRInstruction.Interpreter.Environment (
  IRInterpreterEnv (..),
  insertBoundVars,
  inValueEnv,
) where

import Noll.Common.Environment (Environment (..))
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Utils (Name)

import qualified Noll.Common.Environment as Environment

data IRInterpreterEnv = IRInterpreterEnv
  { irInterpreterValueEnv :: Environment IRValue
  , irInterpreterConstructorEnv :: Environment Int
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE inValueEnv #-}
inValueEnv :: (Environment IRValue -> Environment IRValue) -> IRInterpreterEnv -> IRInterpreterEnv
inValueEnv f IRInterpreterEnv{..} = IRInterpreterEnv{irInterpreterValueEnv = f irInterpreterValueEnv, ..}

{-# INLINE insertBoundVars #-}
insertBoundVars :: [(Name, IRValue)] -> IRInterpreterEnv -> IRInterpreterEnv
insertBoundVars = inValueEnv . Environment.insertMultiple
