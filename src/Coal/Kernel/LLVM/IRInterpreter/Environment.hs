{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Kernel.LLVM.IRInterpreter.Environment (
  IRInterpreterEnv (..),
  inValueEnv,
  inConstructorEnv,
  inIRTypes,
  insertBoundVars,
  objectEnvironment,
  objectConstructors,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Kernel.LLVM.IRType (IRType, IRTyped (..))
import Coal.Kernel.LLVM.IRType.Syntax (opaqueFunction)
import Coal.Kernel.LLVM.IRValue (IRValue (..))
import Coal.Kernel.Language.Object (Object (..), ObjectList, objectConstructorInfo, objectName)
import qualified Data.Text as Text
import Extras (Name, Over)

data IRInterpreterEnv = IRInterpreterEnv
  { irInterpreterValueEnv :: Environment IRValue
  , irInterpreterConstructorEnv :: Environment Int
  , irInterpreterIRTypes :: Environment IRType
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE inValueEnv #-}
inValueEnv :: Over IRInterpreterEnv (Environment IRValue)
inValueEnv f IRInterpreterEnv{..} = IRInterpreterEnv{irInterpreterValueEnv = f irInterpreterValueEnv, ..}

{-# INLINE inConstructorEnv #-}
inConstructorEnv :: Over IRInterpreterEnv (Environment Int)
inConstructorEnv f IRInterpreterEnv{..} = IRInterpreterEnv{irInterpreterConstructorEnv = f irInterpreterConstructorEnv, ..}

{-# INLINE inIRTypes #-}
inIRTypes :: Over IRInterpreterEnv (Environment IRType)
inIRTypes f IRInterpreterEnv{..} = IRInterpreterEnv{irInterpreterIRTypes = f irInterpreterIRTypes, ..}

{-# INLINE insertBoundVars #-}
insertBoundVars :: [(Name, IRValue)] -> IRInterpreterEnv -> IRInterpreterEnv
insertBoundVars = inValueEnv . Environment.insertMultiple

objectEnvironment :: Environment IRType -> ObjectList -> Environment IRValue
objectEnvironment irTypes = foldr (uncurry Environment.insert . objectValue) mempty
 where
  objectValue o =
    let name = objectName o
        irType =
          case o of
            OFunction _ lls _ ->
              opaqueFunction (length lls)
            OConstant _ e ->
              irTypeOf e
            OData _ _ t ->
              irTypeOf t
            OExternal n _ ->
              case Environment.lookup n irTypes of
                Just it ->
                  it
                Nothing ->
                  error $ "IRType not found for external: " <> Text.unpack n
     in (name, Global irType name)

objectConstructors :: ObjectList -> Environment Int
objectConstructors = Environment.fromList . concatMap objectConstructorInfo
