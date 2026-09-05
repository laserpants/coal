{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Package.Version (
  PackageVersion (..),
  PackageConstraint (..),
  AvailableVersion (..),
) where

import CLI.Git.Commit (GitCommit (..))
import Data.Aeson
import Data.SemVer (Version)
import qualified Data.SemVer as SemVerVersion
import Data.SemVer.Constraint (Constraint (..))
import qualified Data.SemVer.Constraint as SemVerConstraint
import Data.Text (Text)
import GHC.Generics (Generic)

newtype PackageVersion = PackageVersion {getVersion :: Version}
  deriving (Generic, Show, Eq, Ord)

newtype PackageConstraint = PackageConstraint {getConstraint :: Constraint}
  deriving (Generic, Show, Eq)

instance ToJSON PackageVersion where
  toJSON (PackageVersion v) =
    String (SemVerVersion.toText v)

instance FromJSON PackageVersion where
  parseJSON =
    withText "PackageVersion" $
      \t ->
        case SemVerVersion.fromText t of
          Left e ->
            fail ("Invalid version: " <> e)
          Right v ->
            pure (PackageVersion v)

instance ToJSON PackageConstraint where
  toJSON (PackageConstraint c) = String (constraintToText c)

constraintToText :: Constraint -> Text
constraintToText = \case
  CAny -> "*"
  CLt v -> "<" <> SemVerVersion.toText v
  CLtEq v -> "<=" <> SemVerVersion.toText v
  CGt v -> ">" <> SemVerVersion.toText v
  CGtEq v -> ">=" <> SemVerVersion.toText v
  CEq v -> SemVerVersion.toText v
  CAnd c1 c2 -> constraintToText c1 <> " " <> constraintToText c2
  COr c1 c2 -> constraintToText c1 <> " || " <> constraintToText c2

instance FromJSON PackageConstraint where
  parseJSON =
    withText "PackageConstraint" $
      \t ->
        case SemVerConstraint.fromText t of
          Left e ->
            fail ("Invalid constraint: " <> e)
          Right v ->
            pure (PackageConstraint v)

data AvailableVersion = AvailableVersion
  { availableVersion :: PackageVersion
  , availableCommit :: GitCommit
  }
  deriving (Show, Eq)

instance Ord AvailableVersion where
  compare (AvailableVersion v1 _) (AvailableVersion v2 _) =
    compare v1 v2
