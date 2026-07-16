{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Package.Lock (
  LockSpec (..),
  PackageLock (..),
  loadLockFile,
) where

import Package.Error (PackageError (..))
import Package.Lock.Spec (LockSpec (..))
import Control.Monad.Except
import Data.Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import System.Directory

newtype PackageLock = PackageLock
  { packages :: Map Text LockSpec
  }
  deriving (Show, Eq, Generic)

instance ToJSON PackageLock where
  toJSON = genericToJSON defaultOptions

instance FromJSON PackageLock where
  parseJSON = genericParseJSON defaultOptions

loadLockFile :: ExceptT PackageError IO (Maybe PackageLock)
loadLockFile = do
  exists <- liftIO $ doesFileExist lockFile
  if exists
    then do
      bytes <- liftIO $ LazyByteString.readFile lockFile
      case eitherDecode bytes of
        Left err ->
          throwError $ EProjectLockFileInvalid (Text.pack err)
        Right file ->
          pure (Just file)
    else pure Nothing
 where
  lockFile = "coal.lock.json"
