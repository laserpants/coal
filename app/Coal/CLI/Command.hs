module Coal.CLI.Command (Command (..)) where

import Coal.CLI.Options.CompileCmd (CompileCmdOptions (..))

data Command
  = CmdCompile CompileCmdOptions
  | CmdBuild
  | CmdClean
  | CmdInstall
  deriving (Show)
