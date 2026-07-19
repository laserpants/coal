{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module CLI.Error (CLIError (..), prettyCLIError) where

import Data.Text (Text)
import Package.Error (PackageError (..), prettyPackageError)

data CLIError
  = EPackageError PackageError
  | EGitError Text
  | EIOError
  | EProjectAlreadyExists
  deriving (Show)

prettyCLIError :: CLIError -> Text
prettyCLIError =
  \case
    EPackageError pkgError ->
      prettyPackageError pkgError
    EGitError err ->
      "Git error:\n\n" <> err
    EIOError ->
      "IO Error"
    EProjectAlreadyExists ->
      "A coal.json file already exists in the current directory.\n\nUse --force to overwrite it."
