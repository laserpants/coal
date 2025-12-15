{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Version (coalVersion) where

import Control.Exception (SomeException, catch)
import Language.Haskell.TH (runIO, stringE)
import System.Process (readProcess)
import Utils (trim)

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
