module Coal.Debugging (writeDebugFile) where

import Data.Text (Text)
import qualified Data.Text.IO as Text
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

writeDebugFile :: FilePath -> Text -> IO ()
writeDebugFile path content = do
  createDirectoryIfMissing True (takeDirectory path)
  Text.writeFile path content
