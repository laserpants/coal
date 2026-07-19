module CLI.Command (Command (..)) where

import CLI.Options.CompileCmd (CompileCmdOptions (..))
import CLI.Options.InitCmd (InitCmdOptions (..))

data Command
  = CmdCompile CompileCmdOptions
  | CmdBuild
  | CmdClean
  | CmdInstall
  | CmdInit InitCmdOptions
  deriving (Show)
