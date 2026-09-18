module CLI.Options.UpdateCmd (UpdateCmdOptions (..)) where

import Data.Text (Text)

data UpdateCmdOptions = UpdateCmdOptions
  { updateTargets :: [Text]
  }
  deriving (Show)
