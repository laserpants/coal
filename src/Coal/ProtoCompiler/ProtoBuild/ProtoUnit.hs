{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.ProtoCompiler.ProtoBuild.ProtoUnit (
  ProtoBuildUnit (..),
  unitPrincipalPath,
) where

import Coal.Language.Module.Path (principalPath)
import Coal.ProtoCompiler.ProtoBuild (ProtoBuild (..))
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))
import Extras (Name)

data ProtoBuildUnit a
  = FromSource a
  | Cached (ProtoBuild a)
  deriving (Show, Eq, Ord)

unitPrincipalPath :: ProtoBuildUnit (ProtoModule a k t) -> Name
unitPrincipalPath =
  principalPath
    . \case
      FromSource ProtoModule{..} ->
        protoOmodulePath
      Cached ProtoBuild{..} ->
        protoObuildPath
