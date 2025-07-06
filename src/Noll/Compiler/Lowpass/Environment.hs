{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Noll.Compiler.Lowpass.Environment (
  TranslateEnvironment (..),
  initialTranslateEnvironment,
  qualifyName,
  withLocalName,
  withLocalNames,
  withModuleName,
  insertQualifiedNames,
) where

import Control.Monad.Reader (MonadReader, ask, local)
import Lang.Common.Environment (Environment)
import Lang.Utils (Name, Over, Set)

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Lang.Common.Environment as Environment

data TranslateEnvironment = TranslateEnvironment
  { translateEnvironmentModule :: Name
  , translateEnvironmentLocalNames :: Set Name
  , translateEnvironmentQualifiedNames :: Environment Name
  }
  deriving (Show, Eq, Ord)

initialTranslateEnvironment :: Environment Name -> TranslateEnvironment
initialTranslateEnvironment = TranslateEnvironment mempty mempty

overTranslateEnvironmentModule :: Over TranslateEnvironment Name
overTranslateEnvironmentModule fn TranslateEnvironment{..} =
  TranslateEnvironment{translateEnvironmentModule = fn translateEnvironmentModule, ..}

overTranslateEnvironmentLocalNames :: Over TranslateEnvironment (Set Name)
overTranslateEnvironmentLocalNames fn TranslateEnvironment{..} =
  TranslateEnvironment{translateEnvironmentLocalNames = fn translateEnvironmentLocalNames, ..}

overTranslateEnvironmentQualifiedNames :: Over TranslateEnvironment (Environment Name)
overTranslateEnvironmentQualifiedNames fn TranslateEnvironment{..} =
  TranslateEnvironment{translateEnvironmentQualifiedNames = fn translateEnvironmentQualifiedNames, ..}

qualifyName :: (MonadReader TranslateEnvironment m) => Name -> m Name
qualifyName name = do
  TranslateEnvironment{..} <- ask
  if name == "_" || Text.head name == '$' || Text.isPrefixOf "Core$" name || Set.member name translateEnvironmentLocalNames
    then pure name
    else case Environment.lookup name translateEnvironmentQualifiedNames of
      Just qname ->
        pure qname
      Nothing ->
        pure (translateEnvironmentModule <> "." <> name)

withLocalName :: (MonadReader TranslateEnvironment m) => Name -> m a -> m a
withLocalName = local . overTranslateEnvironmentLocalNames . Set.insert

withLocalNames :: (Foldable f, MonadReader TranslateEnvironment m) => f Name -> m a -> m a
withLocalNames = flip (foldr withLocalName)

withModuleName :: (MonadReader TranslateEnvironment m) => Name -> m a -> m a
withModuleName = local . overTranslateEnvironmentModule . const

insertQualifiedNames :: (MonadReader TranslateEnvironment m) => Environment Name -> m a -> m a
insertQualifiedNames names = local (overTranslateEnvironmentQualifiedNames (names <>))
