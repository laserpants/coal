{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.TypePhase.TypeInference (passTypeInference) where

import Coal.Compiler.Pass
import Coal.Compiler.Stack
import Coal.Compiler.TypeInference (typeDefinitionsC)
import Coal.Language
import Coal.Language.Module
import Coal.TypeSystem.Substitution
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (gets, runState)
import Data.Data (Data)

passTypeInference :: (MonadIO m, Data a, Eq a, Show a) => Pass a m (Module a Kind ()) (Module a Kind IndexedType)
passTypeInference =
  Pass
    { passName = "TypeInference"
    , runPass = pass
    }

pass :: (MonadIO m, Data a, Eq a, Show a) => Module a Kind () -> CompilerT a m (Module a Kind IndexedType)
pass = runTypeInference

indexedC :: (Monad m, Traversable t) => t e -> CompilerT a m (t IndexedType)
indexedC ds = run (indexed ds) =<< gets compilerSupply
 where
  run s m = do
    let (r, n) = runState s m
    insertSupplyC n
    pure r

runTypeInference :: (MonadIO m, Data a, Eq a, Show a) => Module a Kind () -> CompilerT a m (Module a Kind IndexedType)
runTypeInference m = do
  defs <- traverse indexedC ds
  (tdefs, _) <- typeDefinitionsC defs
  pure (Module p ns (normalizeTypeIndexes tdefs))
 where
  Module p ns ds = m
