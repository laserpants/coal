{-# LANGUAGE StrictData #-}

module Coal.Package.Entry (PackageEntry (..)) where

import Coal.Package.Lock (LockSpec (..))
import Coal.Package.Manifest (PackageManifest (..))
import Data.Text (Text)

data PackageEntry = PackageEntry
  { packageName :: Text
  , packageSpec :: LockSpec
  , packageManifest :: PackageManifest
  }
  deriving (Show, Eq)
