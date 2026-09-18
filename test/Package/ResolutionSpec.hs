{-# LANGUAGE OverloadedStrings #-}

module Package.ResolutionSpec (resolutionSpec) where

import CLI.Git.Commit (GitCommit (..))
import CLI.Git.Repo (GitRepo (..))
import Data.Aeson (eitherDecode, encode)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Package.Lock.Spec (LockSpec (..))
import Package.Resolution (
  LockChange (..),
  LockMode (..),
  ResolutionSource (..),
  chooseSource,
  lockDiff,
  prettyLockChange,
 )
import Package.Version (PackageConstraint, PackageVersion)
import Test.Hspec (Spec, describe, it, shouldBe)

constraint :: Text -> PackageConstraint
constraint txt =
  case eitherDecode (encode txt) of
    Right c -> c
    Left e -> error ("Invalid test constraint: " <> e)

parseVersion :: Text -> PackageVersion
parseVersion txt =
  case eitherDecode (encode txt) of
    Right v -> v
    Left e -> error ("Invalid test version: " <> e)

lockSpec :: Text -> Text -> Text -> LockSpec
lockSpec versionTxt repo commit =
  LockSpec
    { version = parseVersion versionTxt
    , source = GitRepo repo
    , commit = GitCommit commit
    }

microTestRepo :: Text
microTestRepo = "https://codeberg.org/laserpants/coal-micro-test.git"

forkRepo :: Text
forkRepo = "https://example.com/fork.git"

lock :: [(Text, LockSpec)] -> Map.Map Text LockSpec
lock = Map.fromList

resolutionSpec :: Spec
resolutionSpec = do
  describe "chooseSource" $ do
    it "resolves fresh when the lock is ignored" $
      chooseSource LockIgnored False (lock [("pkg", spec)]) "pkg" (GitRepo microTestRepo) Nothing
        `shouldBe` FreshResolve

    it "resolves fresh when the package is absent from the lock" $
      chooseSource LockPreferred False (lock []) "pkg" (GitRepo microTestRepo) Nothing
        `shouldBe` FreshResolve

    it "reuses the locked entry when there is no constraint" $
      chooseSource LockPreferred False (lock [("pkg", spec)]) "pkg" (GitRepo microTestRepo) Nothing
        `shouldBe` FromLock spec

    it "reuses the locked entry when the constraint is satisfied" $
      chooseSource LockPreferred False (lock [("pkg", spec)]) "pkg" (GitRepo microTestRepo) (Just (constraint ">=0.9.0"))
        `shouldBe` FromLock spec

    it "reports a stale lock when the constraint is violated" $
      chooseSource LockPreferred False (lock [("pkg", spec)]) "pkg" (GitRepo microTestRepo) (Just (constraint "0.10.0"))
        `shouldBe` LockStale spec (constraint "0.10.0")

    it "re-resolves when the locked source is a different repository" $
      chooseSource LockPreferred False (lock [("pkg", spec)]) "pkg" (GitRepo forkRepo) (Just (constraint ">=0.9.0"))
        `shouldBe` FreshResolve

    it "re-resolves every dependency of a refreshed package" $
      chooseSource LockPreferred True (lock [("pkg", spec)]) "pkg" (GitRepo microTestRepo) (Just (constraint ">=0.9.0"))
        `shouldBe` FreshResolve

    it "re-resolves a package named in the refresh set" $
      chooseSource (LockRefresh (names ["pkg"])) False (lock [("pkg", spec)]) "pkg" (GitRepo microTestRepo) (Just (constraint ">=0.9.0"))
        `shouldBe` FreshResolve

    it "prefers the lock for packages outside the refresh set" $
      chooseSource (LockRefresh (names ["other"])) False (lock [("pkg", spec)]) "pkg" (GitRepo microTestRepo) (Just (constraint ">=0.9.0"))
        `shouldBe` FromLock spec

    it "refreshes rather than reporting staleness for refreshed packages" $
      chooseSource (LockRefresh (names ["pkg"])) False (lock [("pkg", spec)]) "pkg" (GitRepo microTestRepo) (Just (constraint "0.10.0"))
        `shouldBe` FreshResolve

  describe "lockDiff" $ do
    it "reports no changes for identical locks" $
      lockDiff base base `shouldBe` []

    it "reports added packages" $
      lockDiff base (lock [("pkg", spec), ("extra", other)])
        `shouldBe` [ChangeAdded "extra" other]

    it "reports removed packages" $
      lockDiff (lock [("pkg", spec), ("extra", other)]) base
        `shouldBe` [ChangeRemoved "extra" other]

    it "reports version bumps" $
      lockDiff base (lock [("pkg", bumped)])
        `shouldBe` [ChangeBumped "pkg" spec bumped]

    it "reports a re-pinned commit that keeps the same version" $
      lockDiff base (lock [("pkg", repinned)])
        `shouldBe` [ChangeBumped "pkg" spec repinned]

    it "orders changes by package name" $
      lockDiff base (lock [("pkg", spec), ("aaa", other), ("zzz", other)])
        `shouldBe` [ChangeAdded "aaa" other, ChangeAdded "zzz" other]

  describe "prettyLockChange" $ do
    it "renders additions" $
      prettyLockChange (ChangeAdded "pkg" spec) `shouldBe` "+ pkg 0.9.0"

    it "renders removals" $
      prettyLockChange (ChangeRemoved "pkg" spec) `shouldBe` "- pkg 0.9.0"

    it "renders version bumps" $
      prettyLockChange (ChangeBumped "pkg" spec bumped) `shouldBe` "  pkg 0.9.0 -> 0.10.0"

    it "renders re-pinned commits" $
      prettyLockChange (ChangeBumped "pkg" spec repinned)
        `shouldBe` "  pkg 0.9.0 re-pinned 6731d739 -> 9be8dd5f"
 where
  spec = lockSpec "0.9.0" microTestRepo "6731d7394b8a361e6a883b46af6a6e01002064e0"
  bumped = lockSpec "0.10.0" microTestRepo "9be8dd5f7d75c8acd631bcb58d868322f27265a3"
  repinned = lockSpec "0.9.0" microTestRepo "9be8dd5f7d75c8acd631bcb58d868322f27265a3"
  other = lockSpec "0.4.0" microTestRepo "aaaaaaaa"
  base = lock [("pkg", spec)]
  names :: [Text] -> Set Text
  names = Set.fromList
