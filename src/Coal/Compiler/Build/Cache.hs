{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Build.Cache (cachedData, cachedBuild, writeBuildFile) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Hash256 (Hash256 (..))
import Coal.Compiler.Config (CompilerConfig (..))
import Coal.Compiler.Stack
import Coal.ProtoCompiler.ProtoBuild
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT (..))
import Control.Exception (SomeException (..), try)
import Control.Monad (unless)
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

cachedData :: (MonadIO m) => Name -> CompilerT Metadata (ProtoCompilerT m Metadata) (Either SomeException ByteString)
cachedData name = liftIO $ try $ ByteString.readFile ("./.build" </> Text.unpack name <.> "coal.b")

cachedBuild :: (MonadIO m, Binary a) => Name -> Text -> CompilerT Metadata (ProtoCompilerT m Metadata) (Maybe (ProtoBuild a))
cachedBuild name src = do
  res <- cachedData name
  case res of
    Left{} ->
      pure Nothing
    Right bs ->
      case decodeOrFail (fromStrict bs) of
        Left{} ->
          pure Nothing
        Right (_, _, ProtoBuild{..}) ->
          pure $
            if (unHash256 <$> protoObuildHash) == Just (hash (Text.encodeUtf8 src))
              then Just ProtoBuild{..}
              else Nothing

writeBuildFile :: (MonadIO m, Binary a) => FilePath -> Name -> ProtoBuild a -> CompilerT Metadata (ProtoCompilerT m Metadata) ()
writeBuildFile buildDir name build = do
  CompilerConfig{..} <- gets compilerConfig
  liftIO $ do
    unless configSilent $
      putStrLn file
    ByteString.writeFile (buildDir </> file) (toStrict (encode build))
 where
  file = Text.unpack name <.> "coal.b"
