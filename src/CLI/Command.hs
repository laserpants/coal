module CLI.Command (Command (..)) where

import CLI.Options.AddCmd (AddCmdOptions (..))
import CLI.Options.CompileCmd (CompileCmdOptions (..))
import CLI.Options.InitCmd (InitCmdOptions (..))
import CLI.Options.UpdateCmd (UpdateCmdOptions (..))

data Command
  = CmdAdd AddCmdOptions
  | CmdCompile CompileCmdOptions
  | CmdBuild
  | CmdClean
  | CmdInstall
  | CmdInit InitCmdOptions
  | CmdUpdate UpdateCmdOptions
  deriving (Show)
