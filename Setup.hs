import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Directory
import System.Process

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
  version <- readProcess "git" ["describe", "--tags", "--dirty", "--always"] ""
  writeFile "src/Coal/Version.hs" $
    unlines
      [ "module Coal.Version where"
      , ""
      , "version :: String"
      , "version = " ++ show (trim version)
      ]
 where
  trim = reverse . dropWhile (`elem` "\n ") . reverse . dropWhile (`elem` "\n ")
