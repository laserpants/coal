module Coal.DebugIO (writeDebugFile) where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

writeDebugFile :: FilePath -> Text.Text -> IO ()
writeDebugFile path content = do
  createDirectoryIfMissing True (takeDirectory path)
  Text.writeFile path content
