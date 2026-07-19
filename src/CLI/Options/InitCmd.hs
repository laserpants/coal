module CLI.Options.InitCmd (InitCmdOptions (..)) where

import Data.Text (Text)

-- Define the options for the 'init' command here
data InitCmdOptions = InitCmdOptions
  { initName :: Maybe Text
  , initForce :: Bool
  }
  deriving (Show)
