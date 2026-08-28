{-# LANGUAGE DeriveGeneric #-}

module CLI.Git.Repo (GitRepo (..)) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

newtype GitRepo = GitRepo {repoUrl :: Text}
  deriving (Show, Eq, Ord, Generic)

instance ToJSON GitRepo where
  toJSON = String . repoUrl

instance FromJSON GitRepo where
  parseJSON = fmap GitRepo . parseJSON
