{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.Command.Update (updateCommand) where

import CLI.Command.Install (installProject)
import CLI.Error (CLIError)
import CLI.Options.UpdateCmd (UpdateCmdOptions (..))
import Coal.Compiler.Terminal (TerminalCapabilities)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Package.Resolution (LockMode (..), prettyLockChange)

{- | Re-resolve dependencies to the newest versions allowed by the project
manifest and rewrite the lockfile.

With no arguments every package is re-resolved. Named packages (and their
dependencies) are re-resolved while every other package stays at its
locked version, so a single dependency can be bumped without disturbing
the rest of the graph.
-}
updateCommand :: TerminalCapabilities -> UpdateCmdOptions -> ExceptT CLIError IO ()
updateCommand caps UpdateCmdOptions{updateTargets} = do
  changes <- installProject caps (modeFromTargets updateTargets)
  liftIO $
    case changes of
      [] -> Text.putStrLn "No changes."
      _ -> mapM_ (Text.putStrLn . prettyLockChange) changes

modeFromTargets :: [Text] -> LockMode
modeFromTargets [] = LockIgnored
modeFromTargets targets = LockRefresh (Set.fromList targets)
