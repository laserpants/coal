{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoBuild.ProtoPrep where

import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT (..))
import Coal.ProtoCompiler.ProtoBuild (ProtoBuild (..))

protoOprepareBuild :: ProtoModule a t -> ProtoCompilerT m ProtoBuild
protoOprepareBuild ProtoModule{..} = do
  undefined
