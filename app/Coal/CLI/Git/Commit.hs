{-# LANGUAGE DeriveGeneric #-}

module Coal.CLI.Git.Commit (GitCommit (..)) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

newtype GitCommit = GitCommit {commitHash :: Text}
  deriving (Show, Eq, Ord, Generic)

instance ToJSON GitCommit where
  toJSON = String . commitHash

instance FromJSON GitCommit where
  parseJSON = fmap GitCommit . parseJSON
