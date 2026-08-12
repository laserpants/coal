module CLI.Command (Command (..)) where

import CLI.Options.CompileCmd (CompileCmdOptions (..))
import CLI.Options.InitCmd (InitCmdOptions (..))
import CLI.Options.InstallCmd (InstallCmdOptions (..))

data Command
  = CmdCompile CompileCmdOptions
  | CmdBuild
  | CmdClean
  | CmdInstall InstallCmdOptions
  | CmdInit InitCmdOptions
  deriving (Show)
