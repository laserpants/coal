{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.TranslationPhase.DenormalizeObjects (passDenormalizeObjects) where

import Coal.AST.Normalization
import Coal.Compiler.Pass
import Coal.Language.Module
import Coal.Language.Type (Type (..))
import Data.Data (Data, Typeable)

passDenormalizeObjects :: (Monad m, Monoid a, Data a, Data k, Data (o k), Typeable o) => Pass a m (Module a k (Type o k)) (Module a k (Type o k))
passDenormalizeObjects =
  Pass
    { passName = "DenormalizeObjects"
    , runPass = pure . denormalizeObject
    }
