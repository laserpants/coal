{-# LANGUAGE StrictData #-}

module Package.Entry (PackageEntry (..)) where

import Package.Lock (LockSpec (..))
import Package.Manifest (PackageManifest (..))
import Data.Text (Text)

data PackageEntry = PackageEntry
  { packageName :: Text
  , packageSpec :: LockSpec
  , packageManifest :: PackageManifest
  }
  deriving (Show, Eq)
