{-# LANGUAGE DeriveGeneric #-}

module Package.Version (
  PackageVersion (..),
  PackageConstraint (..),
  AvailableVersion (..),
) where

import CLI.Git.Commit (GitCommit (..))
import Data.Aeson
import Data.SemVer
import qualified Data.SemVer as SemVerVersion
import Data.SemVer.Constraint
import qualified Data.SemVer.Constraint as SemVerConstraint
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
  toJSON (PackageConstraint c) =
    case c of
      CEq v ->
        toJSON (PackageVersion v)
      _ ->
        error "TODO"

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
