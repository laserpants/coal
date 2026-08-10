{-# LANGUAGE TypeApplications #-}

import Control.Exception (SomeException, try)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Directory
import System.Process
import System.Environment (lookupEnv)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { preBuild =
          \args flags -> do
            generateVersion
            preBuild simpleUserHooks args flags
      }

generateVersion :: IO ()
generateVersion = do
  envVersion <- lookupEnv "COAL_VERSION"

  version <-
    case envVersion of
      Just v -> pure v
      Nothing -> do
        e <- try @SomeException
          (readProcess "git" ["describe", "--tags", "--dirty", "--always"] "")
        pure $ either (const "unknown") trim e

  writeFile "src/Coal/Version.hs" $
    unlines
      [ "module Coal.Version (version) where"
      , ""
      , "version :: String"
      , "version = " ++ show version
      ]
  where
    trim = reverse . dropWhile (`elem` "\n ") . reverse . dropWhile (`elem` "\n ")
