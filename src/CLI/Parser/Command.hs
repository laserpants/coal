module CLI.Parser.Command (commandParser) where

import CLI.Command (Command (..))

import CLI.Parser.CompileCmd (compileCmdParser)
import CLI.Parser.InitCmd (initCmdParser)
import Options.Applicative

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command
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
    )
