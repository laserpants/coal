{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Build.Envelope (BuildEnvelope (..), envelopePathName, partitionBuildEnvelopes) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Language.Module.Path
import Coal.ProtoCompiler.ProtoBuild
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))
import Extras (Name)

data BuildEnvelope a
  = BSource a
  | BCached (ProtoBuild Metadata)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

envelopePathName :: BuildEnvelope (ProtoModule a k ()) -> Name
envelopePathName =
  \case
    BSource ProtoModule{..} ->
      principalPath protoOmodulePath
    BCached ProtoBuild{..} ->
      principalPath protoObuildPath

partitionBuildEnvelopes :: [BuildEnvelope a] -> ([a], [ProtoBuild Metadata])
partitionBuildEnvelopes = foldr (flip go) ([], [])
 where
  go (sources, cached) =
    \case
      BSource source ->
        (source : sources, cached)
      BCached b ->
        (sources, b : cached)
