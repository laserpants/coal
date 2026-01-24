{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoBuild.ProtoPrep where

import Coal.ProtoCompiler.ProtoBuild (ProtoBuild (..))
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT (..))
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))

protoOprepareBuild :: ProtoModule a k t -> ProtoCompilerT m ProtoBuild
protoOprepareBuild ProtoModule{..} = do
  undefined

-- collect type constructors
--
--
