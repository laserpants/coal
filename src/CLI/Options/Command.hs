module CLI.Options.Command (CommandOptions (..)) where

import CLI.Command (Command)

{- | The top-level parsed command line.

A single optional, global project-directory override ('-C' / '--directory')
is recorded here and applied by 'Main' before the subcommand runs. Because
every project command reads 'coal.json', 'coal.lock.json' and '.coal/'
relative to the current working directory, channelling the override through
'setCurrentDirectory' makes the flag uniform across 'install', 'update',
'build', 'add' and 'clean' without any per-command plumbing.
-}
data CommandOptions = CommandOptions
  { commandDir :: Maybe String
  , command :: Command
  }
  deriving (Show)
