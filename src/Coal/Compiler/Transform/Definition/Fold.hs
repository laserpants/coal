{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Transform.Definition.Fold where

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

compileTopLevelFolds :: (Monad m) => Definition a k t -> m (Definition a k t)
compileTopLevelFolds =
  \case
    DFold name cs _ -> do
      e1 <- expandTopLevelFold name cs
      pure $ DFold name cs (Just e1)
    o ->
      pure o

expandTopLevelFold :: Name -> NonEmpty (Clause a t) -> m (Constant Expression a t)
expandTopLevelFold name cs =
  undefined
