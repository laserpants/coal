{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Package.ConstraintValidationSpec (constraintValidationSpec) where

import CLI.Command.Install (Violation (..), validateLockEntries, violationsToErrors)
import CLI.Git.Commit (GitCommit (..))
import CLI.Git.Repo (GitRepo (..))
import Data.Aeson (eitherDecode, encode)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Package.Error (ConflictHint (..), PackageError (..), Requirement (..), prettyPackageError)
import Package.Lock.Spec (LockSpec (..))
import Package.Version (PackageConstraint, PackageVersion)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

{- | Parse a constraint in @coal.json@ syntax, failing the test on bad input.
This keeps fixtures concise; constraint parsing itself is covered by
'Package.VersionSpec'.
-}
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

requirement :: Text -> Maybe Text -> Text -> Requirement
requirement source constraintTxt repo =
  Requirement
    { requirementSource = source
    , requirementConstraint = constraint <$> constraintTxt
    , requirementRepo = GitRepo repo
    }

lockSpec :: Text -> Text -> Text -> LockSpec
lockSpec versionTxt repo commit =
  LockSpec
    { version = parseVersion versionTxt
    , source = GitRepo repo
    , commit = GitCommit commit
    }

microTestRepo :: Text
microTestRepo = "https://codeberg.org/laserpants/coal-micro-test.git"

prettyRepo :: Text
prettyRepo = "https://codeberg.org/laserpants/coal-pretty.git"

toViolation :: Requirement -> Violation
toViolation req =
  Violation
    { violationSource = requirementSource req
    , violationConstraint = requirementConstraint req
    , violationRepo = requirementRepo req
    }

constraintValidationSpec :: Spec
constraintValidationSpec =
  describe "Constraint validation" $ do
    it "accepts requirements the locked version satisfies" $ do
      let reqs =
            Map.fromList
              [
                ( "coal-micro-test"
                ,
                  [ requirement "coal-json" (Just "0.10.0") microTestRepo
                  , requirement "coal-parsers" (Just ">=0.9.0") microTestRepo
                  ]
                )
              ]
          lock = Map.fromList [("coal-micro-test", lockSpec "0.10.0" microTestRepo "aaa")]
      validateLockEntries reqs lock `shouldBe` Map.empty
      violationsToErrors HintEditManifests reqs lock (validateLockEntries reqs lock) `shouldBe` []

    it "accepts absent constraints" $ do
      let reqs =
            Map.fromList
              [("coal-containers", [requirement "coal-variant" Nothing microTestRepo])]
          lock = Map.fromList [("coal-containers", lockSpec "0.4.0" microTestRepo "bbb")]
      validateLockEntries reqs lock `shouldBe` Map.empty

    it "reports exact pins that the locked version violates, with requesters" $ do
      -- Mirrors packages/coal-json: the project pins 0.10.0 while
      -- coal-variant pins 0.9.0, and the lock ended up at 0.9.0.
      let reqs =
            Map.fromList
              [
                ( "coal-micro-test"
                ,
                  [ requirement "coal-json" (Just "0.10.0") microTestRepo
                  , requirement "coal-variant" (Just "0.9.0") microTestRepo
                  ]
                )
              ]
          spec = lockSpec "0.9.0" microTestRepo "6731d739"
          lock = Map.fromList [("coal-micro-test", spec)]
          violations = validateLockEntries reqs lock
          expected =
            Map.fromList [("coal-micro-test", [toViolation (requirement "coal-json" (Just "0.10.0") microTestRepo)])]
      violations `shouldBe` expected
      violationsToErrors HintEditManifests reqs lock violations
        `shouldBe` [ EVersionConstraintConflict
                       HintEditManifests
                       "coal-micro-test"
                       (parseVersion "0.9.0")
                       [requirement "coal-json" (Just "0.10.0") microTestRepo]
                       [requirement "coal-variant" (Just "0.9.0") microTestRepo]
                   ]

    it "reports range constraints the locked version violates" $ do
      let reqs =
            Map.fromList
              [("coal-pretty", [requirement "coal-datetime" (Just "0.4.2") prettyRepo])]
          lock = Map.fromList [("coal-pretty", lockSpec "0.5.1" prettyRepo "ccc")]
      validateLockEntries reqs lock `shouldSatisfy` (not . Map.null)

    it "reports requirements pointing at a different repository" $ do
      let reqs =
            Map.fromList
              [("coal-micro-test", [requirement "some-fork" (Just "*") "https://example.com/fork.git"])]
          lock = Map.fromList [("coal-micro-test", lockSpec "0.10.0" microTestRepo "aaa")]
      validateLockEntries reqs lock `shouldSatisfy` (not . Map.null)

    it "collects violations for every conflicting package" $ do
      let reqs =
            Map.fromList
              [ ("a", [requirement "root" (Just "1.0.0") microTestRepo])
              , ("b", [requirement "root" (Just "2.0.0") prettyRepo])
              ]
          lock =
            Map.fromList
              [ ("a", lockSpec "1.0.1" microTestRepo "aaa")
              , ("b", lockSpec "2.0.0" prettyRepo "bbb")
              ]
          violations = validateLockEntries reqs lock
      Map.keys violations `shouldBe` ["a"]
      violationsToErrors HintEditManifests reqs lock violations `shouldSatisfy` ((== 1) . length)

    it "renders both sides of the conflict and a compatibility hint" $ do
      prettyPackageError
        ( EVersionConstraintConflict
            HintEditManifests
            "coal-micro-test"
            (parseVersion "0.9.0")
            [requirement "coal-json" (Just "0.10.0") microTestRepo]
            [requirement "coal-variant" (Just "0.9.0") microTestRepo]
        )
        `shouldBe` ( "Conflicting version requirements for package 'coal-micro-test':\n\n"
                       <> "- 'coal-json' requires '0.10.0' from "
                       <> microTestRepo
                       <> ", but 0.9.0 was selected (not satisfied)"
                       <> "\n\nOther requirements on 'coal-micro-test':\n\n"
                       <> "- 'coal-variant' requires '0.9.0' from "
                       <> microTestRepo
                       <> " (satisfied)"
                       <> "\n\nNo lockfile was written. Update the conflicting dependency declarations or choose compatible package releases. Only relax a constraint after checking compatibility."
                   )

    it "points at coal update when the lockfile is the thing to refresh" $ do
      prettyPackageError
        ( EVersionConstraintConflict
            HintRunUpdate
            "coal-micro-test"
            (parseVersion "0.9.0")
            [requirement "coal-json" (Just "0.10.0") microTestRepo]
            []
        )
        `shouldSatisfy` Text.isInfixOf "Run `coal update`"

    it "renders a stale lockfile as a refresh problem" $ do
      prettyPackageError
        ( EStaleLock
            "coal-micro-test"
            (requirement "coal-json" (Just "0.10.0") microTestRepo)
            (parseVersion "0.9.0")
        )
        `shouldBe` ( "The lockfile is out of date for package 'coal-micro-test':\n\n"
                       <> "- 'coal-json' requires '0.10.0' from "
                       <> microTestRepo
                       <> "\n- coal.lock.json pins 0.9.0"
                       <> "\n\nRun `coal update` to re-resolve the dependency graph."
                   )
