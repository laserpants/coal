{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Coal.Compiler.Build.Envelope

Build envelope for distinguishing source and cached modules.

Wraps modules as either fresh source code to compile or cached build artifacts,
enabling incremental compilation.
-}
module Coal.Compiler.Build.Envelope (
  BuildEnvelope (..),
  envelopePathName,
  partitionBuildEnvelopes,
) where

import Coal.Compiler.Build (Build (..))
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (principalPath)
import Extras (Name)

data BuildEnvelope a
  = BSource a
  | BCached (Build Metadata)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

envelopePathName :: BuildEnvelope (Module a k ()) -> Name
envelopePathName =
  \case
    BSource Module{..} ->
      principalPath modulePath
    BCached Build{..} ->
      principalPath buildPath

partitionBuildEnvelopes :: [BuildEnvelope a] -> ([a], [Build Metadata])
partitionBuildEnvelopes = foldr (flip go) ([], [])
 where
  go (sources, cached) =
    \case
      BSource source ->
        (source : sources, cached)
      BCached b ->
        (sources, b : cached)
