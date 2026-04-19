{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Build.Cache (cachedData, cachedBuild, writeBuildFile) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build
import Coal.Compiler.Build.Hash256 (Hash256 (..))
import Coal.Compiler.Config (CompilerConfig (..))
import Coal.Compiler.Stack
import Coal.Compiler.State
import Control.Exception (SomeException (..), try)
import Control.Monad (guard, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets)
import Crypto.Hash
import Data.Binary (Binary (..), decodeOrFail, encode)
import Data.ByteString (ByteString, fromStrict, toStrict)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Extras (Name)
import System.FilePath ((<.>), (</>))

-- Build cache constants
buildCacheDir :: FilePath
buildCacheDir = ".build"

buildFileExt :: String
buildFileExt = "coal.b"

cachedData :: (MonadIO m) => Name -> CompilerT Metadata m (Either SomeException ByteString)
cachedData name = liftIO $ try $ ByteString.readFile (buildCacheDir </> Text.unpack name <.> buildFileExt)

cachedBuild :: (MonadIO m, Binary a) => Name -> Text -> CompilerT Metadata m (Maybe (Build a))
cachedBuild name src = do
  res <- cachedData name
  pure $ do
    bs <- case res of
      Left{} ->
        Nothing
      Right b ->
        Just b
    case decodeOrFail (fromStrict bs) of
      Left{} ->
        Nothing
      Right (_, _, build) -> do
        let expectedHash = hash (Text.encodeUtf8 src)
        guard (buildHash build == Just (Hash256 expectedHash))
        Just build

writeBuildFile :: (MonadIO m, Binary a) => FilePath -> Name -> Build a -> CompilerT Metadata m ()
writeBuildFile buildDir name build = do
  CompilerConfig{..} <- gets compilerConfig
  liftIO $ do
    unless configSilent $
      putStrLn file
    ByteString.writeFile (buildDir </> file) (toStrict (encode build))
 where
  file :: FilePath
  file = Text.unpack name <.> buildFileExt
