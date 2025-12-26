{-# LANGUAGE DeriveGeneric #-}

module Coal.CLI.Git.Repo (GitRepo (..)) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

-- TODO:
-- Should be something like:
--    data GitRepo = GitSsh .. | GitHttps ..
--
newtype GitRepo = GitRepo {repoUrl :: Text}
  deriving (Show, Eq, Ord, Generic)

instance ToJSON GitRepo where
  toJSON = String . repoUrl

instance FromJSON GitRepo where
  parseJSON = fmap GitRepo . parseJSON
