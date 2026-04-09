{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

module Coal.Compiler.Build.Unit (BuildUnit (..), unitPathName, partitionBuildUnits) where

import Coal.ProtoCompiler.ProtoBuild
import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build
import Coal.Language.Module.Path
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))
import Extras (Name)

data BuildUnit a
  = BSource a
  | 
    -- BCached ModuleBuild
    BCached (ProtoBuild Metadata)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

unitPathName :: BuildUnit (ProtoModule a k ()) -> Name
unitPathName =
  \case
    BSource m ->
      principalPath (protoOmodulePath m)
    BCached b ->
      principalPath (protoObuildPath b)

partitionBuildUnits :: [BuildUnit a] -> ([a], [ProtoBuild Metadata])
partitionBuildUnits = foldr (flip go) ([], [])
 where
  go (sources, cached) =
    \case
      BSource m ->
        (m : sources, cached)
      BCached b ->
        (sources, b : cached)
