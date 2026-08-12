module CLI.Options.AddCmd (AddCmdOptions (..)) where

import Data.Text (Text)

data AddCmdOptions = AddCmdOptions
  { addUrl :: String
  , addVersion :: Maybe Text
  , addName :: Maybe Text
  }
  deriving (Show)
