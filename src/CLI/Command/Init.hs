{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Command.Init (initCommand) where

import CLI.Error (CLIError (..))
import CLI.Options.InitCmd (InitCmdOptions (..))
import Control.Monad (unless, when)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString (toStrict)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Package.Manifest (PackageManifest (..))
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.FilePath (takeFileName)

initCommand :: InitCmdOptions -> ExceptT CLIError IO ()
initCommand InitCmdOptions{..} = do
  -- Check if coal.json already exists
  exists <- liftIO $ doesFileExist "coal.json"
  when exists $
    unless initForce $
      throwError EProjectAlreadyExists

  -- Derive project name from current directory or --name override
  projectName <- case initName of
    Just name -> pure name
    Nothing -> liftIO $ Text.pack . takeFileName <$> getCurrentDirectory

  -- Create src/ directory
  liftIO $ createDirectoryIfMissing True "src"

  -- Write src/Main.coal
  liftIO $ Text.writeFile "src/Main.coal" (mainCoalTemplate projectName)

  -- Write coal.json
  let manifest =
        PackageManifest
          { name = projectName
          , version = Nothing
          , source_dirs = Just ["src"]
          , modules = ["Main"]
          , dependencies = Nothing
          , entry_point = Just "Main.main"
          }
  liftIO $ ByteString.writeFile "coal.json" (toStrict (encodePretty manifest))

  -- Print success message
  liftIO $ Text.putStrLn $ "Initialised project: " <> projectName

mainCoalTemplate :: Text -> Text
mainCoalTemplate _projectName =
  Text.unlines
    [ "module Main {"
    , ""
    , "  import IO(println_string)"
    , ""
    , "  fun main() ="
    , "    println_string(\"Hello, world!\")"
    , ""
    , "}"
    ]