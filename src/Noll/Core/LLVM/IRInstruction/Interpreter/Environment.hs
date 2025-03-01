{-# LANGUAGE RecordWildCards #-}

module Noll.Core.LLVM.IRInstruction.Interpreter.Environment (
  IRInterpreterEnv (..),
  insertBoundVars,
  inValueEnv,
  inConstructorEnv,
) where

import Noll.Common.Environment (Environment (..))
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Utils (Name, Over)

import qualified Noll.Common.Environment as Environment

data IRInterpreterEnv = IRInterpreterEnv
  { irInterpreterValueEnv :: Environment IRValue
  , irInterpreterConstructorEnv :: Environment Int
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE inValueEnv #-}
inValueEnv :: Over IRInterpreterEnv (Environment IRValue)
inValueEnv f IRInterpreterEnv{..} = IRInterpreterEnv{irInterpreterValueEnv = f irInterpreterValueEnv, ..}

{-# INLINE inConstructorEnv #-}
inConstructorEnv :: Over IRInterpreterEnv (Environment Int)
inConstructorEnv f IRInterpreterEnv{..} = IRInterpreterEnv{irInterpreterConstructorEnv = f irInterpreterConstructorEnv, ..}

{-# INLINE insertBoundVars #-}
insertBoundVars :: [(Name, IRValue)] -> IRInterpreterEnv -> IRInterpreterEnv
insertBoundVars = inValueEnv . Environment.insertMultiple
