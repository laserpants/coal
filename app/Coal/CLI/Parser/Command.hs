module Coal.CLI.Parser.Command (commandParser) where

import Coal.CLI.Command (Command (..))
import Coal.CLI.Parser.CompileCmd (compileCmdParser)
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
          -- TODO:
          -- run
          -- update
          -- init
    )
