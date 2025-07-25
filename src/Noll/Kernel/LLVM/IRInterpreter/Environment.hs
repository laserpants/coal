{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.Kernel.LLVM.IRInterpreter.Environment (
  IRInterpreterEnv (..),
  inValueEnv,
  inConstructorEnv,
  insertBoundVars,
  objectEnvironment,
  objectConstructors,
) where

import Lang.Common.Environment (Environment (..))
import Noll.Kernel.LLVM.IRType (IRTyped (..))
import Noll.Kernel.LLVM.IRValue (IRValue (..))
import Noll.Kernel.Language.Object (ObjectList, objectConstructorInfo, objectName)
import Extra (Name, Over)

import qualified Lang.Common.Environment as Environment

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

objectConstructors :: ObjectList -> Environment Int
objectConstructors = Environment.fromList . concatMap objectConstructorInfo
