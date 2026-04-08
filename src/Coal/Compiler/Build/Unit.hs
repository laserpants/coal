{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

module Coal.Compiler.Build.Unit (BuildUnit (..), unitPathName, unitPathName2, partitionBuildUnits) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build
import Coal.Language.Module
import Extras (Name)
import Coal.ProtoLanguage.ProtoModule (ModuleExportList (..), ProtoModule (..))

data BuildUnit a
  = BSource a
  -- PRotobuild
  | BCached (ModuleBuild Metadata)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

unitPathName :: BuildUnit (Module a k ()) -> Name
unitPathName =
  \case
    BSource m ->
      modulePathName m
    BCached b ->
      principalPath (moduleBuildPath b)

unitPathName2 :: BuildUnit (ProtoModule a k ()) -> Name
unitPathName2 =
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
