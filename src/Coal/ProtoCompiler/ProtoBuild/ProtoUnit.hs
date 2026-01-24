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
  = UnitSource a
  | UnitCached ProtoBuild
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

unitPrincipalPath :: ProtoBuildUnit (ProtoModule a k t) -> Name
unitPrincipalPath =
  principalPath
    . \case
      UnitSource ProtoModule{..} ->
        protoOmodulePath
      UnitCached ProtoBuild{..} ->
        protoObuildPath
