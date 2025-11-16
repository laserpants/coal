{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Kernel.LLVM.IRInterpreter.Environment (
  IRInterpreterEnv (..),
  inValueEnv,
  inConstructorEnv,
  insertBoundVars,
  objectEnvironment,
  objectConstructors,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Kernel.LLVM.IRType (IRTyped (..))
import Coal.Kernel.LLVM.IRValue (IRValue (..))
import Coal.Kernel.Language.Object (ObjectList, objectConstructorInfo, objectName)
import Extras (Name, Over)

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
