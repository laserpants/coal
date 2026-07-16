module CLI.Command (Command (..)) where

import CLI.Options.CompileCmd (CompileCmdOptions (..))

data Command
  = CmdCompile CompileCmdOptions
  | CmdBuild
  | CmdClean
  | CmdInstall
  deriving (Show)
