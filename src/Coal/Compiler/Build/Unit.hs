{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

module Coal.Compiler.Build.Unit (BuildUnit (..), unitPathName, partitionBuildUnits) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build
import Coal.Language.Module.Path
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))
import Extras (Name)

data BuildUnit a
  = BSource a
  | -- TODO: ProtoBuild
    BCached (ModuleBuild Metadata)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

unitPathName :: BuildUnit (ProtoModule a k ()) -> Name
unitPathName =
  \case
    BSource m ->
      principalPath (protoOmodulePath m)
    BCached b ->
      principalPath (moduleBuildPath b)

partitionBuildUnits :: [BuildUnit a] -> ([a], [ModuleBuild Metadata])
partitionBuildUnits = foldr (flip go) ([], [])
 where
  go (sources, cached) =
    \case
      BSource m ->
        (m : sources, cached)
      BCached b ->
        (sources, b : cached)
