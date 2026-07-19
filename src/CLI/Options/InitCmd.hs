{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module CLI.Options.InitCmd (InitCmdOptions (..)) where

import Data.Text (Text)

data InitCmdOptions = InitCmdOptions
  { initName :: Maybe Text
  , initForce :: Bool
  }
  deriving (Show)