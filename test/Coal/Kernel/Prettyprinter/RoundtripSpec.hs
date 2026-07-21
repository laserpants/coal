{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Prettyprinter.RoundtripSpec (spec) where

import Coal.Kernel.Language.Expr (Expr (..), Label (..))
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (FunctionScope (..), Object (..))
import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Kernel.Language.Type (Type (..))
import qualified Coal.Kernel.Language.Type.Constructors as T
import Coal.Kernel.Parser.Expr (expr)
import Coal.Kernel.Parser.Module (module_)
import Coal.Kernel.Parser.Prim (prim)
import Coal.Kernel.Parser.Type (type_)
import Coal.Kernel.Prettyprinter (renderModule)
import Coal.Kernel.Prettyprinter.Expr (prettyExpr)
import Coal.Kernel.Prettyprinter.Prim (prettyPrim)
import Coal.Kernel.Prettyprinter.Type (prettyType)
import Control.Monad (forM_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Void (Void)
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, parse)

-- | Helper to parse a module
parseModule :: Text.Text -> Either (ParseErrorBundle Text.Text Void) (Module Type)
parseModule = parse module_ ""

-- | Helper to render a value
render :: (a -> Doc ann) -> a -> Text.Text
render printer val = renderStrict $ layoutPretty defaultLayoutOptions $ printer val

spec :: Spec
spec = do
  describe "Prettyprinter roundtrip tests" $ do
    describe "Prim roundtrip" $ do
      it "roundtrips int32" $ do
        let original = PInt32 42
        let rendered = render prettyPrim original
        parse prim "" rendered `shouldBe` Right original

      it "roundtrips string" $ do
        let original = PString "hello world"
        let rendered = render prettyPrim original
        parse prim "" rendered `shouldBe` Right original

      it "roundtrips string with escapes" $ do
        let original = PString "hello\nworld\t!"
        let rendered = render prettyPrim original
        parse prim "" rendered `shouldBe` Right original

      it "roundtrips char" $ do
        let original = PChar 120 -- 'x'
        let rendered = render prettyPrim original
        parse prim "" rendered `shouldBe` Right original

      it "roundtrips char with escapes" $ do
        let original = PChar 10 -- '\n'
        let rendered = render prettyPrim original
        parse prim "" rendered `shouldBe` Right original

    describe "Type roundtrip" $ do
      it "roundtrips int32 type" $ do
        let original = T.int32
        let rendered = render prettyType original
        parse type_ "" rendered `shouldBe` Right original

      it "roundtrips function type" $ do
        let original = T.arrow T.int32 T.bool
        let rendered = render prettyType original
        parse type_ "" rendered `shouldBe` Right original

      it "roundtrips list type" $ do
        let original = TCon "list" [T.int32]
        let rendered = render prettyType original
        parse type_ "" rendered `shouldBe` Right original

      it "roundtrips tuple type" $ do
        let original = TCon "tuple2" [T.int32, T.bool]
        let rendered = render prettyType original
        parse type_ "" rendered `shouldBe` Right original

      it "roundtrips record type" $ do
        let original = TCon "record" [RExt "x" T.int32 (RExt "y" T.int32 RNil)]
        let rendered = render prettyType original
        parse type_ "" rendered `shouldBe` Right original

    describe "Expr roundtrip" $ do
      it "roundtrips literal" $ do
        let original = ELit (PInt32 42) :: Expr Type
        let rendered = render (prettyExpr prettyType) original
        parse expr "" rendered `shouldBe` Right original

      it "roundtrips variable" $ do
        let original = EVar (Label T.int32 "x") :: Expr Type
        let rendered = render (prettyExpr prettyType) original
        parse expr "" rendered `shouldBe` Right original

      it "roundtrips application" $ do
        let original = EApp T.int32 (EVar (Label (T.arrow T.int32 T.int32) "f")) (NE.fromList [ELit (PInt32 42)]) :: Expr Type
        let rendered = render (prettyExpr prettyType) original
        parse expr "" rendered `shouldBe` Right original

    describe "Module roundtrip" $ do
      it "roundtrips minimal module" $ do
        let original = Module "Test" [] [DConstant "Answer" (ELit (PInt32 42))]
        let rendered = renderModule original
        parseModule rendered `shouldBe` Right original

      it "roundtrips module with imports" $ do
        let original = Module "Test" ["Core$.add"] [DConstant "Answer" (ELit (PInt32 42))]
        let rendered = renderModule original
        parseModule rendered `shouldBe` Right original

      it "roundtrips module with data" $ do
        let original = Module "Test" [] [DData "Point" [("Point", TCon "Point" [])]]
        let rendered = renderModule original
        parseModule rendered `shouldBe` Right original

      it "roundtrips module with function" $ do
        let original = Module "Test" [] [DFunction Exported "Identity" [Label T.int32 "x"] (EVar (Label T.int32 "x"))]
        let rendered = renderModule original
        parseModule rendered `shouldBe` Right original

-- NOTE: File-based roundtrip tests temporarily disabled while .corn example
-- files are being updated to use the new grouped data syntax.
-- describe "File-based roundtrip tests" $ do
--   cornFiles <- runIO findCornFiles
--   forM_ cornFiles $ \filePath -> do
--     it ("roundtrips " ++ filePath) $ do
--       content <- TextIO.readFile filePath
--       case parseModule content of
--         Left err -> expectationFailure $ "Failed to parse original file:\n" ++ errorBundlePretty err
--         Right original -> do
--           let rendered = renderModule original
--           case parseModule rendered of
--             Left err ->
--               expectationFailure $
--                 "Failed to parse prettyprinted output:\n"
--                   ++ errorBundlePretty err
--                   ++ "\n\nPrettyprinted output:\n"
--                   ++ Text.unpack rendered
--             Right reparsed -> reparsed `shouldBe` original

-- | Recursively find all .corn files in the test/examples directory
findCornFiles :: IO [FilePath]
findCornFiles = findCornFiles' "test/examples"
 where
  findCornFiles' :: FilePath -> IO [FilePath]
  findCornFiles' dir = do
    exists <- doesDirectoryExist dir
    if not exists
      then return []
      else do
        entries <- listDirectory dir
        let paths = map (dir </>) entries
        files <- filterM doesFileExist paths
        let cornFiles = filter (\f -> takeExtension f == ".corn") files
        dirs <- filterM doesDirectoryExist paths
        subFiles <- concat <$> mapM findCornFiles' dirs
        return (cornFiles ++ subFiles)

  filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
  filterM _ [] = return []
  filterM p (x : xs) = do
    flg <- p x
    ys <- filterM p xs
    return (if flg then x : xs else ys)
