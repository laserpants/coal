{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Coal.Package.Dependency (PackageDependency (..)) where

import Coal.CLI.Git.Repo (GitRepo (..))
import Coal.Package.Version (PackageConstraint (..))
import Data.Aeson
import GHC.Generics (Generic)

data PackageDependency = PackageDependency
  { version :: Maybe PackageConstraint
  , git :: GitRepo
  }
  deriving (Generic, Show, Eq)

instance ToJSON PackageDependency where
  toJSON = genericToJSON defaultOptions

instance FromJSON PackageDependency where
  parseJSON = genericParseJSON defaultOptions
