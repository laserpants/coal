{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.CLI.Git (
  gitLsRemoteHash,
  gitCloneRepo,
  gitCheckoutCommit,
  gitLsRemoteVersions,
) where

import Coal.CLI.Error (CLIError (..))
import Coal.CLI.Git.Commit (GitCommit (..))
import Coal.CLI.Git.Repo (GitRepo (..))
import Coal.Package.Version (AvailableVersion (..), PackageVersion (..))
import Control.Monad.Except
import Data.Char (isSpace)
import Data.Either.Extra (eitherToMaybe)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.SemVer (fromText)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Exit (ExitCode (..))
import System.Process

gitLsRemoteHash :: GitRepo -> ExceptT CLIError IO GitCommit
gitLsRemoteHash (GitRepo url) = do
  (exit, out, err) <- liftIO $ readCreateProcessWithExitCode process []
  case exit of
    ExitSuccess ->
      pure (GitCommit (extractHash out))
    ExitFailure{} ->
      throwError (EGitError (Text.pack err))
 where
  process = proc "git" ["ls-remote", Text.unpack url, "HEAD"]
  extractHash str = Text.takeWhile (not . isSpace) (Text.pack str)

gitCloneRepo :: GitRepo -> FilePath -> ExceptT CLIError IO ()
gitCloneRepo (GitRepo url) path = do
  (exit, _, err) <- liftIO $ readCreateProcessWithExitCode process []
  case exit of
    ExitSuccess ->
      pure ()
    ExitFailure{} ->
      throwError (EGitError (Text.pack err))
 where
  process = proc "git" ["clone", Text.unpack url, path]

gitCheckoutCommit :: GitCommit -> FilePath -> ExceptT CLIError IO ()
gitCheckoutCommit (GitCommit hash) path = do
  (exit, _, err) <- liftIO $ readCreateProcessWithExitCode process []
  case exit of
    ExitSuccess ->
      liftIO $ Text.putStrLn hash
    ExitFailure{} ->
      throwError (EGitError (Text.pack err))
 where
  process = (proc "git" ["checkout", Text.unpack hash]){cwd = Just path}

gitLsRemoteVersions :: GitRepo -> ExceptT CLIError IO [AvailableVersion]
gitLsRemoteVersions (GitRepo url) = do
  (exit, out, err) <- liftIO $ readCreateProcessWithExitCode process []
  case exit of
    ExitSuccess ->
      liftIO $ pure (sortOn Down (availableVersionsFromLsRemote (Text.pack out)))
    ExitFailure{} ->
      throwError (EGitError (Text.pack err))
 where
  process = proc "git" ["ls-remote", "--tags", Text.unpack url]

data RemoteRef = RemoteRef
  { refHash :: GitCommit
  , refName :: Text
  }
  deriving (Show, Eq)

parseLsRemoteLine :: Text -> Maybe RemoteRef
parseLsRemoteLine line =
  case Text.words line of
    [hash, ref] -> Just (RemoteRef (GitCommit hash) ref)
    _ -> Nothing

collectTagCommits :: [RemoteRef] -> Map PackageVersion GitCommit
collectTagCommits refs =
  Map.fromListWith prefer $
    mapMaybe toEntry refs
 where
  toEntry :: RemoteRef -> Maybe (PackageVersion, GitCommit)
  toEntry ref = do
    (tag, _) <- isTagRef ref
    ver <- parseVersionTag tag
    pure (ver, refHash ref)

  -- Prefer dereferenced commits over tag objects
  prefer :: GitCommit -> GitCommit -> GitCommit
  prefer new _old = new

isTagRef :: RemoteRef -> Maybe (Text, Bool)
isTagRef RemoteRef{refName} =
  case Text.stripPrefix "refs/tags/" refName of
    Just rest
      | Just tag <- Text.stripSuffix "^{}" rest ->
          Just (tag, True) -- dereferenced commit
      | otherwise ->
          Just (rest, False) -- lightweight or tag object
    Nothing -> Nothing

availableVersionsFromLsRemote :: Text -> [AvailableVersion]
availableVersionsFromLsRemote output =
  map toAvailable (Map.toList tagCommits)
 where
  refs =
    mapMaybe parseLsRemoteLine (Text.lines output)
  tagCommits =
    collectTagCommits refs
  toAvailable (ver, commit) =
    AvailableVersion ver commit

parseVersionTag :: Text -> Maybe PackageVersion
parseVersionTag tag = do
  let clean = fromMaybe tag (Text.stripPrefix "v" tag)
  PackageVersion <$> (eitherToMaybe . fromText) clean
