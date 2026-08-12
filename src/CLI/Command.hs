module CLI.Command (Command (..)) where

import CLI.Options.AddCmd (AddCmdOptions (..))
import CLI.Options.CompileCmd (CompileCmdOptions (..))
import CLI.Options.InitCmd (InitCmdOptions (..))
import CLI.Options.InstallCmd (InstallCmdOptions (..))

data Command
  = CmdAdd AddCmdOptions
  | CmdCompile CompileCmdOptions
  | CmdBuild
  | CmdClean
  | CmdInstall InstallCmdOptions
  | CmdInit InitCmdOptions
  deriving (Show)
