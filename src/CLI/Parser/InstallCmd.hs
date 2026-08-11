module CLI.Parser.InstallCmd (installCmdParser) where

import CLI.Options.InstallCmd (InstallCmdOptions (..))
import Options.Applicative

installCmdParser :: Parser InstallCmdOptions
installCmdParser =
  InstallCmdOptions
    <$> strOption
          ( long "target"
              <> metavar "PATH"
              <> value "."
              <> help "Target directory (defaults to current directory)"
          )
    <*> optional
          ( strOption
              ( long "revision"
                  <> metavar "REV"
                  <> help "Specific Git revision to install"
              )
          )
