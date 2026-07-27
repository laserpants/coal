{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module CLI.Command.Version (coalVersion) where

import Control.Exception (SomeException, catch)
import Data.List.Extra (trim)
import Language.Haskell.TH (litE, runIO, stringL)
import System.Environment (lookupEnv)
import System.Process (readProcess)

coalVersion :: String
coalVersion =
  $( do
      result <-
        runIO $
          ( do
              envVal <- lookupEnv "COAL_VERSION"
              case envVal of
                Just v -> pure v
                Nothing ->
                  trim
                    <$> readProcess
                      "git"
                      ["describe", "--tags", "--dirty", "--always"]
                      ""
          )
            `catch` \(_ :: SomeException) ->
              pure "unknown"
      litE (stringL result)
   )
