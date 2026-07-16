{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Package.Dependency (PackageDependency (..)) where

import CLI.Git.Repo (GitRepo (..))
import Data.Aeson
import GHC.Generics (Generic)
import Package.Version (PackageConstraint (..))

data PackageDependency = PackageDependency
  { version :: Maybe PackageConstraint
  , git :: GitRepo
  }
  deriving (Generic, Show, Eq)

instance ToJSON PackageDependency where
  toJSON = genericToJSON defaultOptions

instance FromJSON PackageDependency where
  parseJSON = genericParseJSON defaultOptions
