{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Transform.Definition.Unfold where

import Coal.Common.Label (Label (..), labelName)
import Coal.Common.Supply (suppliedName)
import Coal.Compiler.Transform.Expression
import Coal.Compiler.Transform.Flattening (flattenApplication)
import Coal.Compiler.Transform.Tree (replace)
import Coal.Language (Choice (..), Clause (..), Expression (..), Pattern (..))
import Coal.Language.Module (Constant (..), Definition (..), Function (..), Module (..))
import Control.Monad.RWS (RWS, runRWS)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (execWriter, tell)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transform, transformM)
import Data.List.NonEmpty (NonEmpty (..))
import Extra (Dictionary, Name, const2)

-- TODO

compileTopLevelUnfolds :: (Monad m) => Definition a k t -> m (Definition a k t)
compileTopLevelUnfolds =
  \case
    DAnnotation t d ->
      DAnnotation t <$> compileTopLevelUnfolds d
    DUnfold name ps d _ -> do
      e1 <- expandTopLevelUnfold name ps d
      pure $ DUnfold name ps d (Just e1)
    o ->
      pure o

expandTopLevelUnfold :: Name -> NonEmpty (Pattern a t) -> Dictionary (Expression a t) -> m (Expression a t)
expandTopLevelUnfold name ps d =
  undefined
