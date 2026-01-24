{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

module Coal.ProtoCompiler.ProtoBuild.ProtoUnit where

import Coal.ProtoCompiler.ProtoBuild (ProtoBuild)
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))
import Extras (Name)

data ProtoBuildUnit a
  = UInput a
  | UCached ProtoBuild
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

unitPathName :: ProtoBuildUnit ProtoModule -> Name
unitPathName =
  \case
    UInput m ->
      modulePathName m
    UCached b ->
      principalPath (moduleBuildPath b)

