module Coal.CLI.Command.Clean (cleanCommand) where

import Control.Exception (catch)
import System.Directory
import System.IO.Error (isDoesNotExistError)

cleanCommand :: IO ()
cleanCommand =
  removeDirectoryRecursive ".build"
    `catch` \e ->
      if isDoesNotExistError e
        then pure ()
        else ioError e
