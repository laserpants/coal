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
  deriving (Show)

prettyCLIError :: CLIError -> Text
prettyCLIError =
  \case
    EPackageError pkgError ->
      prettyPackageError pkgError
    EGitError err ->
      "• Git error:\n\n" <> err
    EIOError ->
      "IO Error"
