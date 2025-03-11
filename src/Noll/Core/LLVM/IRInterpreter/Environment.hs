{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.LLVM.IRInterpreter.Environment (
  IRInterpreterEnv (..),
  inValueEnv,
  inConstructorEnv,
  insertBoundVars,
  objectEnvironment,
) where

import Noll.Common.Environment (Environment (..))
import Noll.Core.LLVM.IRType (IRTyped (..))
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Core.Language.Object (Object (..), ObjectList, objectName)
import Noll.Utils (Name, Over, listenOnly)

import qualified Noll.Common.Environment as Environment
import qualified Noll.Core.Language as Core

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

objectEnvironment :: ObjectList -> Environment IRValue
objectEnvironment = foldr (uncurry Environment.insert . objectValue) mempty
 where
  objectValue o = let name = objectName o in (name, Global (irTypeOf o) name)
