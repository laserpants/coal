{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.TypePhase.TypeInference (passTypeInference) where

import Coal.Compiler.Build.Core (buildEnv, replacePlaceholders)
import Coal.Compiler.Builtin.Definitions (builtinFunctions)
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Compiler.TypeInference (typeDefinitionsC)
import Coal.Language (IndexedType, Kind, indexed)
import Coal.Language.Module (Module (..), principalPath)
import Coal.TypeSystem.Constraint.Assumption (Assumption (..))
import Coal.TypeSystem.Substitution (normalizeTypeIndexes)
import Control.Monad.Except
import Control.Monad.State (gets, runState)
import Data.Data (Data)
import Data.List (nub)
import qualified Data.Text as Text

passTypeInference :: (MonadIO m, Data a, Eq a, Show a) => Pass a m (Module a Kind ()) (Module a Kind IndexedType)
passTypeInference = Pass{runPass = pass}

pass :: (MonadIO m, Data a, Eq a, Show a) => Module a Kind () -> CompilerT a m (Module a Kind IndexedType)
pass m@(Module path _ _) = do
  env <- buildEnv
  setNamesC env
  insertNamesC builtinFunctions

  next <- runTypeInference m
  names <- gets compilerNameStore
  replacePlaceholders names

  assumptions <- gets (filter (not . isFoldAssumption) . nub . compilerAssumptions)
  forM_ assumptions $
    \Assumption{..} -> do
      tellErrors [NameNotInScope assumptionName (ErrorLocation (principalPath path) assumptionMetadata)]
  unless (null assumptions) $
    throwError NoSuchIdentifier
  pure next

isFoldAssumption :: Assumption a t -> Bool
isFoldAssumption Assumption{..} = "!" `Text.isPrefixOf` assumptionName

indexTypes :: (Monad m, Traversable t) => t e -> CompilerT a m (t IndexedType)
indexTypes ds = run (indexed ds) =<< gets compilerSupply
 where
  run s m = do
    let (r, n) = runState s m
    insertSupplyC n
    pure r

runTypeInference :: (MonadIO m, Data a, Eq a, Show a) => Module a Kind () -> CompilerT a m (Module a Kind IndexedType)
runTypeInference m = do
  defs <- traverse indexTypes ds
  (tdefs, _) <- typeDefinitionsC defs
  pure (Module p ns (normalizeTypeIndexes tdefs))
 where
  Module p ns ds = m
