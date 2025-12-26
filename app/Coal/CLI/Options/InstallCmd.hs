{-# LANGUAGE StrictData #-}

module Coal.CLI.Options.InstallCmd (InstallCmdOptions (..)) where

data InstallCmdOptions = InstallCmdOptions
  { installTarget :: String
  , installRevision :: Maybe String
  }
  deriving (Show)
