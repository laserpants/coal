{-# LANGUAGE DeriveGeneric #-}

module Coal.Compiler.Build.Hash256 (Hash256 (..)) where

import Crypto.Hash
import Data.Binary
import Data.Binary.Get (getByteString)
import Data.Binary.Put (putByteString)
import qualified Data.ByteArray as ByteArray
import GHC.Generics (Generic)

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
