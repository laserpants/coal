{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Package.Resolution (
  LockMode (..),
  ResolutionSource (..),
  LockChange (..),
  chooseSource,
  lockDiff,
  prettyLockChange,
  versionSatisfies,
  repoMatches,
) where

import CLI.Git.Commit (GitCommit (..))
import CLI.Git.Repo (GitRepo (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.SemVer (toText)
import qualified Data.SemVer.Constraint as SemVerConstraint
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Extras (Name)
import Package.Lock.Spec (LockSpec (..))
import Package.Version (PackageConstraint (..), PackageVersion (..))

{- | How much authority the existing lockfile has over resolution.

@LockIgnored@ re-resolves the whole graph (no lockfile, or `coal update`
with no arguments); @LockPreferred@ reuses locked versions whenever they
still satisfy the declaring constraints (the default for `coal install`);
@LockRefresh@ re-resolves the named packages (and their subtrees) while
keeping every other package at its locked version.
-}
data LockMode
  = LockIgnored
  | LockPreferred
  | LockRefresh (Set Name)
  deriving (Show, Eq)

{- | Where a single dependency edge takes its version and commit from.

@LockStale@ means a lockfile entry exists for the package but no longer
satisfies the constraint declared by the manifest that requires it: the
lockfile predates a manifest change and must be refreshed.
-}
data ResolutionSource
  = FromLock LockSpec
  | FreshResolve
  | LockStale LockSpec PackageConstraint
  deriving (Show, Eq)

{- | Decide how to resolve one dependency edge.

The boolean records whether the requesting package is itself being
re-resolved (its subtree is under refresh), so that a refreshed package
brings its dependencies along instead of leaving them pinned.

A package named in a @LockRefresh@ set is always re-resolved. Otherwise a
lockfile entry is reused only when it points at the same repository and
satisfies the declared constraint; a missing entry (or one pointing at a
different repository, i.e. a deliberate source change) is re-resolved,
while a conflicting entry is reported as stale.
-}
chooseSource :: LockMode -> Bool -> Map Name LockSpec -> Name -> GitRepo -> Maybe PackageConstraint -> ResolutionSource
chooseSource mode underRefresh lockEntries pkgName repo constraint
  | mustRefresh = FreshResolve
  | otherwise =
      case Map.lookup pkgName lockEntries of
        Nothing -> FreshResolve
        Just spec
          | not (repoMatches spec repo) -> FreshResolve
          | Just c <- constraint, not (versionSatisfies c (version spec)) -> LockStale spec c
          | otherwise -> FromLock spec
 where
  mustRefresh =
    underRefresh || case mode of
      LockIgnored -> True
      LockPreferred -> False
      LockRefresh names -> pkgName `Set.member` names

-- | Whether a locked package originates from the required repository.
repoMatches :: LockSpec -> GitRepo -> Bool
repoMatches LockSpec{source} repo = source == repo

-- | Whether a version satisfies a constraint.
versionSatisfies :: PackageConstraint -> PackageVersion -> Bool
versionSatisfies (PackageConstraint constraint) (PackageVersion release) =
  SemVerConstraint.satisfies release constraint

{- | A single difference between two lockfiles, as reported by
@coal update@.
-}
data LockChange
  = ChangeAdded Name LockSpec
  | ChangeBumped Name LockSpec LockSpec
  | ChangeRemoved Name LockSpec
  deriving (Show, Eq)

{- | Differences between an old and a new lockfile, ordered by package
name. Entries that are identical in version, source, and commit produce
no change.
-}
lockDiff :: Map Name LockSpec -> Map Name LockSpec -> [LockChange]
lockDiff old new =
  [ change
  | name <- Set.toList (Map.keysSet old `Set.union` Map.keysSet new)
  , Just change <- [changeFor name]
  ]
 where
  changeFor :: Name -> Maybe LockChange
  changeFor name =
    case (Map.lookup name old, Map.lookup name new) of
      (Nothing, Just newSpec) -> Just (ChangeAdded name newSpec)
      (Just oldSpec, Nothing) -> Just (ChangeRemoved name oldSpec)
      (Just oldSpec, Just newSpec)
        | oldSpec /= newSpec -> Just (ChangeBumped name oldSpec newSpec)
        | otherwise -> Nothing
      (Nothing, Nothing) -> Nothing

{- | Render one lockfile change for the @coal update@ summary. Version
bumps are shown as @old -> new@; a re-pinned commit that keeps the same
version is shown as a short commit range.
-}
prettyLockChange :: LockChange -> Text
prettyLockChange = \case
  ChangeAdded name spec ->
    "+ " <> name <> " " <> prettyVersion (version spec)
  ChangeRemoved name spec ->
    "- " <> name <> " " <> prettyVersion (version spec)
  ChangeBumped name oldSpec newSpec
    | version oldSpec /= version newSpec ->
        "  " <> name <> " " <> prettyVersion (version oldSpec) <> " -> " <> prettyVersion (version newSpec)
    | otherwise ->
        "  " <> name <> " " <> prettyVersion (version oldSpec) <> " re-pinned " <> shortCommit oldSpec <> " -> " <> shortCommit newSpec
 where
  prettyVersion (PackageVersion release) = toText release
  shortCommit LockSpec{commit = GitCommit hash} = Text.take 8 hash
