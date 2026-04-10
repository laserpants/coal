{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Build.Unit (BuildUnit (..), unitPathName, partitionBuildUnits) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Language.Module.Path
import Coal.ProtoCompiler.ProtoBuild
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))
import Extras (Name)

data BuildUnit a
  = BSource a
  | BCached (ProtoBuild Metadata)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

unitPathName :: BuildUnit (ProtoModule a k ()) -> Name
unitPathName =
  \case
    BSource ProtoModule{..} ->
      principalPath protoOmodulePath
    BCached ProtoBuild{..} ->
      principalPath protoObuildPath

partitionBuildUnits :: [BuildUnit a] -> ([a], [ProtoBuild Metadata])
partitionBuildUnits = foldr (flip go) ([], [])
 where
  go (sources, cached) =
    \case
      BSource source ->
        (source : sources, cached)
      BCached b ->
        (sources, b : cached)
