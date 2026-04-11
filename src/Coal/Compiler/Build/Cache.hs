{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Build.Cache (cachedData, cachedBuild, writeBuildFile) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build
import Coal.Compiler.Build.Hash256 (Hash256 (..))
import Coal.Compiler.Config (CompilerConfig (..))
import Coal.Compiler.Stack
import Coal.Compiler.State
import Control.Exception (SomeException (..), try)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets)
import Control.Monad.Trans (lift)
import Crypto.Hash
import Data.Binary (Binary (..), decodeOrFail, encode)
import Data.ByteString (ByteString, fromStrict, toStrict)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Extras (Name)
import System.FilePath ((<.>), (</>))

cachedData :: (MonadIO m) => Name -> CompilerT Metadata m (Either SomeException ByteString)
cachedData name = liftIO $ try $ ByteString.readFile ("./.build" </> Text.unpack name <.> "coal.b")

cachedBuild :: (MonadIO m, Binary a) => Name -> Text -> CompilerT Metadata m (Maybe (Build a))
cachedBuild name src = do
  res <- cachedData name
  case res of
    Left{} ->
      pure Nothing
    Right bs ->
      case decodeOrFail (fromStrict bs) of
        Left{} ->
          pure Nothing
        Right (_, _, Build{..}) ->
          pure $
            if (unHash256 <$> protoObuildHash) == Just (hash (Text.encodeUtf8 src))
              then Just Build{..}
              else Nothing

writeBuildFile :: (MonadIO m, Binary a) => FilePath -> Name -> Build a -> CompilerT Metadata m ()
writeBuildFile buildDir name build = do
  CompilerConfig{..} <- gets protoOcompilerConfig
  liftIO $ do
    unless configSilent $
      putStrLn file
    ByteString.writeFile (buildDir </> file) (toStrict (encode build))
 where
  file = Text.unpack name <.> "coal.b"
