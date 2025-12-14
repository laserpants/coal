{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Version (coalVersion) where

import Control.Exception
import Language.Haskell.TH
import System.Process
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
