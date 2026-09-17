{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Command.Install (
  Violation (..),
  installCommand,
  installProject,
  validateLockEntries,
  violatesRequirement,
  violationsToErrors,
) where

import CLI.Error (CLIError (..))
import CLI.Git (gitCheckoutCommit, gitCloneRepo, gitLsRemoteVersions)
import CLI.Git.Commit (GitCommit (..))
import CLI.Git.Repo (GitRepo (..))
import Coal.Compiler.Terminal (TerminalCapabilities, sanitizeForTerminal)
import Control.Monad (unless, when)
import Control.Monad.Except
import Control.Monad.State
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString (toStrict)
import qualified Data.ByteString as ByteString
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.SemVer (toText)
import Data.SemVer.Constraint (Constraint (CAny))
import qualified Data.SemVer.Constraint as SemVerConstraint (satisfies)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Extras (Name, Over, forM_)
import Package.Dependency (PackageDependency (..))
import Package.Error (PackageError (..), Requirement (..))
import Package.Lock (PackageLock (..))
import Package.Lock.Spec (LockSpec (..))
import Package.Manifest
import Package.Version (AvailableVersion (..), PackageConstraint (..), PackageVersion (..), getConstraint)
import System.Directory (doesDirectoryExist)
import System.IO (hPutStrLn, stderr)
import TextShow (showt)

data InstallState = InstallState
  { visited :: Set (Name, GitCommit)
  , lockEntries :: Map Name LockSpec
  , requirements :: Map Name [Requirement]
  , caps :: TerminalCapabilities
  }
  deriving (Show, Eq)

overVisited :: Over InstallState (Set (Name, GitCommit))
overVisited fn InstallState{..} = InstallState{visited = fn visited, ..}

overLockEntries :: Over InstallState (Map Name LockSpec)
overLockEntries fn InstallState{..} = InstallState{lockEntries = fn lockEntries, ..}

overRequirements :: Over InstallState (Map Name [Requirement])
overRequirements fn InstallState{..} = InstallState{requirements = fn requirements, ..}

{-# INLINE initialInstallState #-}
initialInstallState :: TerminalCapabilities -> InstallState
initialInstallState = InstallState mempty mempty mempty

addVisited :: (Name, GitCommit) -> StateT InstallState (ExceptT CLIError IO) ()
addVisited pkg = modify (overVisited (Set.insert pkg))

addLockEntry :: Name -> LockSpec -> StateT InstallState (ExceptT CLIError IO) ()
addLockEntry name spec = modify (overLockEntries (Map.insert name spec))

addRequirement :: Name -> Requirement -> StateT InstallState (ExceptT CLIError IO) ()
addRequirement name requirement =
  modify (overRequirements (Map.insertWith (<>) name [requirement]))

{- | Print a progress line to stderr, degrading gracefully to ASCII on
terminals that don't support Unicode (e.g. when output is piped).
-}
announce :: TerminalCapabilities -> Text -> IO ()
announce caps msg =
  hPutStrLn stderr (Text.unpack (sanitizeForTerminal caps ("• " <> msg)))

{- | 'announce' lifted into the install state monad, reading the terminal
capabilities from the install state.
-}
progress :: Text -> StateT InstallState (ExceptT CLIError IO) ()
progress msg = do
  InstallState{caps = c} <- get
  liftIO $ announce c msg

installPackage :: Name -> PackageVersion -> GitRepo -> GitCommit -> StateT InstallState (ExceptT CLIError IO) ()
installPackage name version repo commit = do
  InstallState{..} <- get
  unless ((name, commit) `elem` visited) $ do
    addVisited (name, commit)
    let dir = basePath name commit
    exists <- liftIO $ doesDirectoryExist dir

    if exists
      then progress ("Already installed: " <> describe name version commit)
      else do
        progress ("Cloning " <> name <> " from " <> repoUrl repo <> "...")
        lift (gitCloneRepo repo dir)
        progress ("Checking out " <> describe name version commit <> "...")
        lift (gitCheckoutCommit commit dir)
        progress ("Installed " <> describe name version commit)

    addLockEntry name LockSpec{version = version, source = repo, commit = commit}
    PackageManifest{dependencies = deps} <- lift (withExceptT EPackageError (loadManifest name commit))
    installDependencies name (fromMaybe mempty deps)

installDependencies :: Name -> Map Text PackageDependency -> StateT InstallState (ExceptT CLIError IO) ()
installDependencies source deps =
  forM_ (Map.toList deps) $
    \(pkgName, PackageDependency{git = repo, version = constraint}) -> do
      addRequirement pkgName Requirement{requirementSource = source, requirementConstraint = constraint, requirementRepo = repo}
      progress ("Resolving versions for " <> pkgName <> " (" <> repoUrl repo <> ")...")
      versions <- lift $ gitLsRemoteVersions repo
      case pickVersionHash constraint versions of
        Nothing -> do
          throwError (EPackageError err)
         where
          err =
            ENoPackageVersionMatch
              pkgName
              (fromMaybe (PackageConstraint CAny) constraint)
              (availableVersion <$> versions)
        Just (AvailableVersion pkgVersion commit) -> do
          progress ("Resolved " <> describe pkgName pkgVersion commit)
          installPackage pkgName pkgVersion repo commit

constraintSatisfies :: PackageConstraint -> AvailableVersion -> Bool
constraintSatisfies (PackageConstraint constraint) (AvailableVersion{availableVersion = PackageVersion version}) =
  SemVerConstraint.satisfies version constraint

pickVersionHash :: Maybe PackageConstraint -> [AvailableVersion] -> Maybe AvailableVersion
pickVersionHash Nothing (v : _) = Just v
pickVersionHash (Just constraint) versions = find (constraintSatisfies constraint) versions
pickVersionHash _ _ = Nothing

{- | A single violated requirement: the requesting manifest, its stated
constraint, and the locked version that fails it.
-}
data Violation = Violation
  { violationSource :: Name
  , violationConstraint :: Maybe PackageConstraint
  , violationRepo :: GitRepo
  }
  deriving (Show, Eq)

{- | Check every collected requirement against the locked versions. Returns
the violated requirements per locked package, so the caller can report
them with their requesters. Resolution itself is unchanged: this only
ever rejects a result, never alters which version wins.
-}
validateLockEntries :: Map Name [Requirement] -> Map Name LockSpec -> Map Name [Violation]
validateLockEntries requirements lockEntries =
  Map.mapMaybeWithKey validatePackage requirements
 where
  validatePackage :: Name -> [Requirement] -> Maybe [Violation]
  validatePackage pkgName reqs =
    case Map.lookup pkgName lockEntries of
      -- Every requirement must resolve to some lock entry: all processed
      -- dependency edges install their target, so a missing entry is a
      -- bug in the walk, not a constraint conflict.
      Nothing -> Nothing
      Just LockSpec{version = PackageVersion{}} ->
        case filter (violatesRequirement lockEntries pkgName) reqs of
          [] -> Nothing
          bad -> Just (toViolation <$> bad)
   where
    toViolation :: Requirement -> Violation
    toViolation req =
      Violation
        { violationSource = requirementSource req
        , violationConstraint = requirementConstraint req
        , violationRepo = requirementRepo req
        }

{- | Whether a single requirement is violated by a lock entry: the locked
repository must match, and the locked version must satisfy the stated
constraint (an absent constraint is always satisfied).
-}
violatesRequirement :: Map Name LockSpec -> Name -> Requirement -> Bool
violatesRequirement lockEntries pkgName Requirement{requirementConstraint, requirementRepo} =
  repoConflict || constraintConflict
 where
  -- A requirement against a different repository can never be
  -- satisfied by this lock entry, whatever versions it lists.
  repoConflict :: Bool
  repoConflict =
    case Map.lookup pkgName lockEntries of
      Just LockSpec{source} -> requirementRepo /= source
      Nothing -> True
  constraintConflict :: Bool
  constraintConflict =
    case (Map.lookup pkgName lockEntries, requirementConstraint) of
      (Just LockSpec{version = PackageVersion locked}, Just constraint) ->
        not (SemVerConstraint.satisfies locked (getConstraint constraint))
      _ ->
        False

{- | Convert per-package violations into 'PackageError's carrying the
locked version alongside the violated requirements, for rendering. The
error also carries the satisfied requirements on the same package, so
the message can show the other side of the conflict.
-}
violationsToErrors :: Map Name [Requirement] -> Map Name LockSpec -> Map Name [Violation] -> [PackageError]
violationsToErrors requirements entries =
  Map.foldMapWithKey toError
 where
  toError :: Name -> [Violation] -> [PackageError]
  toError pkgName violations =
    case Map.lookup pkgName entries of
      Nothing -> []
      Just LockSpec{version} ->
        [EVersionConstraintConflict pkgName version (toRequirement <$> violations) (satisfiedOn pkgName)]
  satisfiedOn :: Name -> [Requirement]
  satisfiedOn pkgName =
    [ req
    | req <- fromMaybe [] (Map.lookup pkgName requirements)
    , not (violatesRequirement entries pkgName req)
    ]
  toRequirement :: Violation -> Requirement
  toRequirement viol =
    Requirement
      { requirementSource = violationSource viol
      , requirementConstraint = violationConstraint viol
      , requirementRepo = violationRepo viol
      }

{- | A short, human-readable description of a resolved package:
'<name>@<version> (<short hash>)'.
-}
describe :: Name -> PackageVersion -> GitCommit -> Text
describe name (PackageVersion version) (GitCommit hash) =
  name <> "@" <> toText version <> " (" <> Text.take 8 hash <> ")"

installProject :: TerminalCapabilities -> ExceptT CLIError IO ()
installProject caps = do
  res <- liftIO $ runExceptT loadProjectManifest
  case res of
    Left err ->
      throwError (EPackageError err)
    Right PackageManifest{..} -> do
      let deps = fromMaybe mempty dependencies
      when (not (Map.null deps)) $
        liftIO $
          announce caps "Resolving project dependencies..."
      InstallState{lockEntries, requirements} <-
        flip execStateT (initialInstallState caps) $
          installDependencies name deps

      case violationsToErrors requirements lockEntries (validateLockEntries requirements lockEntries) of
        [] ->
          pure ()
        err : _ ->
          throwError (EPackageError err)

      liftIO $ do
        ByteString.writeFile "coal.lock.json" (toStrict (encodePretty (PackageLock lockEntries)))
      liftIO $ announce caps ("Wrote coal.lock.json with " <> showt (Map.size lockEntries) <> " packages")

installCommand :: TerminalCapabilities -> ExceptT CLIError IO ()
installCommand = installProject
