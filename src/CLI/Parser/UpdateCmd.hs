module CLI.Parser.UpdateCmd (updateCmdParser) where

import CLI.Options.UpdateCmd (UpdateCmdOptions (..))
import qualified Data.Text as Text
import Options.Applicative

updateCmdParser :: Parser UpdateCmdOptions
updateCmdParser =
  UpdateCmdOptions
    <$> many
      ( Text.pack
          <$> strArgument
            ( metavar "PACKAGE"
                <> help "Package to re-resolve (defaults to every package)"
            )
      )
