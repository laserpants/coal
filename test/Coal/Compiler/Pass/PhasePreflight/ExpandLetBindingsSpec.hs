{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.PhasePreflight.ExpandLetBindingsSpec (expandLetBindingsSpec) where

import Coal.Compiler.Metadata (Metadata)
import Coal.Compiler.Pass.PhasePreflight.ExpandLetBindings (expandLetBindings, expandLetBindingsModule)
import Coal.Language
import Coal.Language.AST.Builders
import Coal.Parser (parseSourceFile)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Test.Hspec
import Text.Megaparsec (parse)

expandLetBindingsSpec :: Spec
expandLetBindingsSpec =
  describe "ExpandLetBindings" $ do
    describe "expandLetBindings" $ do
      it "leaves a single-binding let unchanged" $
        expandLetBindings (letGroup [("a", varE "x")] (varE "a"))
          `shouldBe` letGroup [("a", varE "x")] (varE "a")

      it "rewrites a two-binding let into nested single-binding lets" $
        expandLetBindings (letGroup [("a", varE "x"), ("b", varE "y")] (varE "b"))
          `shouldBe` nestedLet [("a", varE "x"), ("b", varE "y")] (varE "b")

      it "rewrites a three-binding let into a chain of single-binding lets" $
        expandLetBindings (letGroup [("a", varE "x"), ("b", varE "y"), ("c", varE "z")] (varE "c"))
          `shouldBe` nestedLet [("a", varE "x"), ("b", varE "y"), ("c", varE "z")] (varE "c")

      it "expands let groups nested inside binding right-hand sides" $
        expandLetBindings
          ( letGroup
              [("a", letGroup [("x", varE "u"), ("y", varE "v")] (varE "y"))]
              (varE "a")
          )
          `shouldBe` letGroup
            [("a", nestedLet [("x", varE "u"), ("y", varE "v")] (varE "y"))]
            (varE "a")

      it "leaves non-let expressions unchanged" $
        expandLetBindings (applicationE (varE "f") (varE "a" :| []))
          `shouldBe` applicationE (varE "f") (varE "a" :| [])

    describe "expandLetBindingsModule (parse-based)" $ do
      it "expands a multi-binding let group in a parsed module" $ do
        case parse parseSourceFile "" multiBindingSource of
          Left err ->
            expectationFailure (show err)
          Right m -> do
            countLets m `shouldBe` 1
            countLets (expandLetBindingsModule m) `shouldBe` 2

      it "expands a three-binding let group in a parsed module" $ do
        case parse parseSourceFile "" threeBindingSource of
          Left err ->
            expectationFailure (show err)
          Right m -> do
            countLets m `shouldBe` 1
            countLets (expandLetBindingsModule m) `shouldBe` 3

      it "leaves a single-binding let in a parsed module unchanged" $ do
        case parse parseSourceFile "" singleBindingSource of
          Left err ->
            expectationFailure (show err)
          Right m -> do
            countLets m `shouldBe` 1
            countLets (expandLetBindingsModule m) `shouldBe` 1

multiBindingSource :: Text.Text
multiBindingSource =
  Text.unlines
    [ "module Main {"
    , "  fun f(x : int32) : int32 ="
    , "    let a = 1; b = a + 1 in a + b + x"
    , "}"
    ]

threeBindingSource :: Text.Text
threeBindingSource =
  Text.unlines
    [ "module Main {"
    , "  fun f(x : int32) : int32 ="
    , "    let a = 1; b = 2; c = 3 in a + b + c + x"
    , "}"
    ]

singleBindingSource :: Text.Text
singleBindingSource =
  Text.unlines
    [ "module Main {"
    , "  fun f(x : int32) : int32 ="
    , "    let a = 1 in a + x"
    , "}"
    ]

countLets :: Module Metadata () () -> Int
countLets m =
  length (filter isLet (universeBi m :: [Expression Metadata () ()]))
 where
  isLet ELet{} = True
  isLet _ = False

-- | @let a = e1; b = e2 in body@ (the pre-expansion representation)
letGroup :: [(Text.Text, Expression Metadata () ())] -> Expression Metadata () () -> Expression Metadata () ()
letGroup bindings body =
  ELet mempty (NonEmpty.fromList (map (uncurry patternBinding) bindings)) body

-- | @let a = e1 in let b = e2 in body@ (the post-expansion representation)
nestedLet :: [(Text.Text, Expression Metadata () ())] -> Expression Metadata () () -> Expression Metadata () ()
nestedLet bindings body =
  foldr step body (map (uncurry patternBinding) bindings)
 where
  step b acc = ELet mempty (b :| []) acc

patternBinding :: Text.Text -> Expression Metadata () () -> Binding Expression Metadata () ()
patternBinding n = BPattern mempty (varP n)
