{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Package.Error (PackageError (..), prettyPackageError) where

import Data.Text (Text)
import Extras (Name)
import Package.Version (
  PackageConstraint (..),
  PackageVersion (..),
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
  deriving (Show, Eq)

prettyPackageError :: PackageError -> Text
prettyPackageError =
  \case
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
      "The package '" <> name <> "' is missinig a manifest file." <> maybe "" ("\n\nLocation: " <>) text
    EDependencyInvalidManifestFormat name text ->
      "Package '" <> name <> "' manifest file format is invalid:\n\n" <> text
    EDependencyInvalidModuleFormat name moduleName ->
      "Module '" <> moduleName <> "' in the package '" <> name <> "' is not a valid module name."
    ENoPackageVersionMatch name _ _ ->
      "No install candidate found for package '" <> name <> "'"
