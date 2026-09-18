{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Package.Error (PackageError (..), ConflictHint (..), Requirement (..), prettyPackageError) where

import CLI.Git.Commit (GitCommit (..))
import CLI.Git.Repo (GitRepo (..))
import Data.SemVer (Version, toText)
import Data.Text (Text)
import qualified Data.Text as Text
import Extras (Name)
import Package.Version (
  PackageConstraint (..),
  PackageVersion (..),
  prettyPackageConstraint,
 )

data PackageError
  = EProjectManifestMissing
  | EProjectLockFileInvalid Text
  | EProjectInvalidManifestFormat Text
  | EProjectInvalidModuleFormat Name
  | ENoLockFile
  | EDependencyManifestMissing Name (Maybe Text)
  | EDependencyInvalidManifestFormat Name Text
  | EDependencyInvalidModuleFormat Name Name
  | ENoPackageVersionMatch Name PackageConstraint [PackageVersion]
  | EVersionConstraintConflict ConflictHint Name PackageVersion [Requirement] [Requirement]
  | EStaleLock Name Requirement PackageVersion
  | ECommitUnavailable Name GitCommit
  | EUnknownUpdateTarget [Name] [Name]
  deriving (Show, Eq)

{- | What the user should do about a version conflict. A conflict
discovered while re-resolving the whole graph means the manifest
declarations themselves are incompatible; a conflict involving locked
versions means the lockfile predates a manifest change and needs
refreshing.
-}
data ConflictHint
  = HintEditManifests
  | HintRunUpdate
  deriving (Show, Eq)

{- | A single declaration of interest in a package: @source@ names the
manifest that declares the dependency, @constraint@ is its (possibly
absent) version requirement, and @repo@ is the Git repository the
requirement points at.
-}
data Requirement = Requirement
  { requirementSource :: Name
  , requirementConstraint :: Maybe PackageConstraint
  , requirementRepo :: GitRepo
  }
  deriving (Show, Eq)

prettyPackageError :: PackageError -> Text
prettyPackageError = \case
  EProjectManifestMissing ->
    "Project manifest (coal.json) file is missing."
  EProjectLockFileInvalid text ->
    "The project lock-file format is invalid:\n\n" <> text
  EProjectInvalidManifestFormat text ->
    "Project manifest (coal.json) file format is invalid:\n\n" <> text
  EProjectInvalidModuleFormat name ->
    "'" <> name <> "' is not a valid module name."
  ENoLockFile ->
    "No project lock-file found.\n\nTry running `coal install`."
  EDependencyManifestMissing name text ->
    "The package '" <> name <> "' is missing a manifest file." <> maybe "" ("\n\nLocation: " <>) text
  EDependencyInvalidManifestFormat name text ->
    "Package '" <> name <> "' manifest file format is invalid:\n\n" <> text
  EDependencyInvalidModuleFormat name moduleName ->
    "Module '" <> moduleName <> "' in the package '" <> name <> "' is not a valid module name."
  ENoPackageVersionMatch name _ _ ->
    "No install candidate found for package '" <> name <> "'"
  EVersionConstraintConflict hint name (PackageVersion version) violated satisfied ->
    "Conflicting version requirements for package '"
      <> name
      <> "':\n\n"
      <> Text.intercalate "\n" (prettyRequirement version <$> violated)
      <> ( if null satisfied
             then ""
             else
               "\n\nOther requirements on '"
                 <> name
                 <> "':\n\n"
                 <> Text.intercalate "\n" (prettySatisfiedRequirement <$> satisfied)
         )
      <> "\n\n"
      <> conflictFooter hint
  EStaleLock name requirement (PackageVersion locked) ->
    "The lockfile is out of date for package '"
      <> name
      <> "':\n\n- '"
      <> requirementSource requirement
      <> "' requires '"
      <> maybe "*" prettyPackageConstraint (requirementConstraint requirement)
      <> "' from "
      <> repoUrl (requirementRepo requirement)
      <> "\n- coal.lock.json pins "
      <> toText locked
      <> "\n\nRun `coal update` to re-resolve the dependency graph."
  ECommitUnavailable name (GitCommit hash) ->
    "Could not check out commit "
      <> Text.take 8 hash
      <> " for package '"
      <> name
      <> "'.\n\nThe revision may no longer exist upstream (for example after a force-push). Run `coal update` to re-resolve the dependency graph."
  EUnknownUpdateTarget unknown known ->
    "Unknown package(s): "
      <> Text.intercalate ", " unknown
      <> if null known
        then ""
        else "\n\nKnown packages: " <> Text.intercalate ", " known

{- | Closing advice for a version conflict, depending on whether the
manifest declarations or the lockfile are the thing to change.
-}
conflictFooter :: ConflictHint -> Text
conflictFooter = \case
  HintEditManifests ->
    "No lockfile was written. Update the conflicting dependency declarations or choose compatible package releases. Only relax a constraint after checking compatibility."
  HintRunUpdate ->
    "The lockfile is out of date with the project manifest; no lockfile was written. Run `coal update` to re-resolve the dependency graph."

{- | Render one violated requirement: who declared it, which version
constraint it stated, and which locked version it is incompatible with.
-}
prettyRequirement :: Version -> Requirement -> Text
prettyRequirement version req =
  "- '"
    <> requirementSource req
    <> "' requires '"
    <> maybe "*" prettyPackageConstraint (requirementConstraint req)
    <> "' from "
    <> repoUrl (requirementRepo req)
    <> ", but "
    <> toText version
    <> " was selected (not satisfied)"

{- | Render one satisfied requirement: who declared it and which version
constraint it stated.
-}
prettySatisfiedRequirement :: Requirement -> Text
prettySatisfiedRequirement req =
  "- '"
    <> requirementSource req
    <> "' requires '"
    <> maybe "*" prettyPackageConstraint (requirementConstraint req)
    <> "' from "
    <> repoUrl (requirementRepo req)
    <> " (satisfied)"
