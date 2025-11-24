{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.TranslationPhase.NormalizeObjects (passNormalizeObjects) where

import Coal.AST.Normalization (NormalizationContext (normalizeObject))
import Coal.Compiler.Pass (Pass (..))
import Coal.Language.Module (Module)
import Coal.Language.Type (Type (..))
import Data.Data (Data, Typeable)

passNormalizeObjects :: (Monad m, Monoid a, Data a, Data k, Data (o k), Typeable o) => Pass a m (Module a k (Type o k)) (Module a k (Type o k))
passNormalizeObjects =
  Pass
    { passName = "NormalizeObjects"
    , runPass = pure . normalizeObject
    }
