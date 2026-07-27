{-# LANGUAGE ScopedTypeVariables #-}

module CLI.Command.Version (coalVersion) where

import Control.Exception (SomeException, catch)
import Data.List.Extra (trim)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)

coalVersion :: String
coalVersion =
  unsafePerformIO $
    (trim <$> readProcess "git" ["describe", "--tags", "--dirty", "--always"] "")
      `catch` \(_ :: SomeException) ->
        pure "unknown"
{-# NOINLINE coalVersion #-}
