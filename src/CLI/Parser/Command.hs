module CLI.Parser.Command (commandOptionsParser, commandParser) where

import CLI.Command (Command (..))
import CLI.Options.Command (CommandOptions (CommandOptions))
import CLI.Parser.AddCmd (addCmdParser)
import CLI.Parser.CompileCmd (compileCmdParser)
import CLI.Parser.InitCmd (initCmdParser)
import CLI.Parser.UpdateCmd (updateCmdParser)
import Options.Applicative

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command
        "add"
        ( info
            (CmdAdd <$> addCmdParser)
            (progDesc "Add a dependency from a Git repository")
        )
        <> command
          "compile"
          ( info
              (CmdCompile <$> compileCmdParser)
              (progDesc "Compile from source files")
          )
        <> command
          "build"
          ( info
              (pure CmdBuild)
              (progDesc "Build project from manifest")
          )
        <> command
          "clean"
          ( info
              (pure CmdClean)
              (progDesc "Remove build artifacts")
          )
        <> command
          "install"
          ( info
              (pure CmdInstall)
              (progDesc "Install packages from project manifest")
          )
        <> command
          "init"
          ( info
              (CmdInit <$> initCmdParser)
              (progDesc "Initialise a new project")
          )
        <> command
          "update"
          ( info
              (CmdUpdate <$> updateCmdParser)
              (progDesc "Re-resolve dependencies and rewrite the lockfile")
          )
    )

-- The top-level command parser: an optional global '-C' / '--directory'
-- override followed by the chosen subcommand. 'Main' applies the directory via
-- 'setCurrentDirectory' before any command runs, so every project command
-- reads coal.json / .coal / .build relative to the current directory.
commandOptionsParser :: Parser CommandOptions
commandOptionsParser =
  CommandOptions
    <$> dirOption
    <*> commandParser
 where
  dirOption = optional (strOption opts)
  opts =
    long "directory"
      <> short 'C'
      <> metavar "DIR"
      <> help "Run as if coal was started in DIR (applies to all subcommands)"
