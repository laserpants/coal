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
  = UInput a
  | UCached ProtoBuild
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

unitPrincipalPath :: ProtoBuildUnit (ProtoModule a) -> Name
unitPrincipalPath =
  principalPath
    . \case
      UInput ProtoModule{..} ->
        protoOmodulePath
      UCached ProtoBuild{..} ->
        protoObuildPath
