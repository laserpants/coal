{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Coal.Package.Lock.Spec (LockSpec (..)) where

import Coal.CLI.Git.Commit (GitCommit (..))
import Coal.CLI.Git.Repo (GitRepo (..))
import Coal.Package.Version (PackageVersion (..))
import Data.Aeson
import GHC.Generics (Generic)

data LockSpec = LockSpec
  { version :: PackageVersion
  , source :: GitRepo
  , commit :: GitCommit
  }
  deriving (Show, Eq, Generic)

instance ToJSON LockSpec where
  toJSON = genericToJSON defaultOptions

instance FromJSON LockSpec where
  parseJSON = genericParseJSON defaultOptions
