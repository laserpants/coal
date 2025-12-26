{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Coal.CLI.Command.Version (coalVersion) where

import Control.Exception (SomeException, catch)
import Data.List.Extra (trim)
import Language.Haskell.TH (runIO, stringE)
import System.Process (readProcess)

coalVersion :: String
coalVersion =
  $( do
      v <-
        runIO $
          (trim <$> readProcess "git" ["describe", "--tags", "--dirty", "--always"] "")
            `catch` \(_ :: SomeException) ->
              pure "unknown"
      stringE v
   )
