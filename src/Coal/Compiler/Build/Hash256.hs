{-# LANGUAGE DeriveGeneric #-}

{- |
Module: Coal.Compiler.Build.Hash256

SHA256 hash wrapper type for build system integrity verification.

This module provides a newtype wrapper around SHA256 cryptographic digests,
used throughout the build system to verify module integrity and track changes
for incremental compilation. The Hash256 type provides binary serialization
for efficient storage in cache files and comparison operations for change
detection.
-}
module Coal.Compiler.Build.Hash256 (Hash256 (..)) where

import Crypto.Hash (Digest, SHA256, digestFromByteString)
import Data.Binary
import Data.Binary.Get (getByteString)
import Data.Binary.Put (putByteString)
import qualified Data.ByteArray as ByteArray
import GHC.Generics (Generic)

{- | SHA256 cryptographic hash digest.

Wrapper around SHA256 digest used for content-based hashing in the build
system. Provides Binary serialization for cache storage and Eq/Ord instances
for efficient comparison and change detection during incremental compilation.
-}
newtype Hash256 = Hash256 {unHash256 :: Digest SHA256}
  deriving (Eq, Ord, Show, Generic)

instance Binary Hash256 where
  put (Hash256 d) =
    putByteString (ByteArray.convert d)
  get = do
    bs <- getByteString 32
    case digestFromByteString bs of
      Just d ->
        pure (Hash256 d)
      Nothing ->
        fail "Invalid SHA256 digest"
