{-# LANGUAGE StrictData #-}

module Package.Entry (PackageEntry (..)) where

import Data.Text (Text)
import Package.Lock (LockSpec (..))
import Package.Manifest (PackageManifest (..))

data PackageEntry = PackageEntry
  { packageName :: Text
  , packageSpec :: LockSpec
  , packageManifest :: PackageManifest
  }
  deriving (Show, Eq)
