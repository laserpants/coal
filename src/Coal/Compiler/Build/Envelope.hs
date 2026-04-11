-- +
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Build.Envelope (
  BuildEnvelope (..),
  envelopePathName,
  partitionBuildEnvelopes,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path
import Extras (Name)

data BuildEnvelope a
  = BSource a
  | BCached (Build Metadata)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

envelopePathName :: BuildEnvelope (Module a k ()) -> Name
envelopePathName =
  \case
    BSource Module{..} ->
      principalPath protoOmodulePath
    BCached Build{..} ->
      principalPath protoObuildPath

partitionBuildEnvelopes :: [BuildEnvelope a] -> ([a], [Build Metadata])
partitionBuildEnvelopes = foldr (flip go) ([], [])
 where
  go (sources, cached) =
    \case
      BSource source ->
        (source : sources, cached)
      BCached b ->
        (sources, b : cached)
