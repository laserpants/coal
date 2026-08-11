module CLI.Parser.AddCmd (addCmdParser) where

import CLI.Options.AddCmd (AddCmdOptions (..))
import qualified Data.Text as Text
import Options.Applicative

addCmdParser :: Parser AddCmdOptions
addCmdParser =
  AddCmdOptions
    <$> strArgument
          ( metavar "GIT_URL"
              <> help "Git repository URL"
          )
    <*> optional
          ( fmap Text.pack
              ( strOption
                  ( long "version"
                      <> metavar "CONSTRAINT"
                      <> help "SemVer constraint (defaults to *)"
                  )
              )
          )
    <*> optional
          ( fmap Text.pack
              ( strOption
                  ( long "name"
                      <> metavar "NAME"
                      <> help "Package name (defaults to name from package's coal.json)"
                  )
              )
          )