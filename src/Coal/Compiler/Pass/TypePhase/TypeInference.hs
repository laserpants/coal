{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.TypePhase.TypeInference (passTypeInference) where

import qualified Coal.Common.Environment as Environment
import Coal.Common.Name (isConstructor)
import Coal.Compiler.Builtin.Definitions (builtinFunctions, builtinTraitInstances)
import Coal.Compiler.Journal
import Coal.Compiler.Pass
import Coal.Compiler.Stack
import Coal.Compiler.TypeInference (typeDefinitionsC)
import Coal.Language
import Coal.Language.Module
import Coal.TypeSystem.Constraint.Assumption (Assumption (..))
import Coal.TypeSystem.Substitution
import Control.Monad.Except
import Control.Monad.State (gets, runState)
import Data.Data (Data)
import Data.List (nub)
import qualified Data.Text as Text

passTypeInference :: (MonadIO m, Data a, Eq a, Show a) => Pass a m (Module a Kind ()) (Module a Kind IndexedType)
passTypeInference =
  Pass
    { passName = "TypeInference"
    , runPass = pass
    }

-- TODO: Maybe look these up in environment and add additional constraints?
isFoldAssumption :: Assumption a t -> Bool
isFoldAssumption Assumption{..} = "!" `Text.isPrefixOf` assumptionName

processImports :: (Monad m) => Module a Kind () -> CompilerT a m ()
processImports (Module _ _ ds) = do
  env <- gets compilerGlobalNames
  forM_ ds $
    \case
      DImport loc p names -> do
        let pp = principalPath p
        case Environment.lookup pp env of
          Nothing ->
            tellErrors [ModuleNotFound pp (ErrorLocation pp loc)]
          Just moduleNames -> do
            forM_ (filter (not . isConstructor) names) $
              \name ->
                unless (name `elem` builtinTraitInstances || Environment.contains name moduleNames) $ do
                  tellErrors [NameNotInModule name pp (ErrorLocation pp loc)]
                  throwError PreflightFailure
            insertNamesC (Environment.lookupAll names moduleNames)
      _ ->
        pure ()

pass :: (MonadIO m, Data a, Eq a, Show a) => Module a Kind () -> CompilerT a m (Module a Kind IndexedType)
pass m@(Module p xs _) = do
  setCompilerModuleC p
  clearAssumptionsC
  clearNameStoreC
  insertNamesC builtinFunctions
  insertGlobalNamesC "Builtin$" (Environment.fromList builtinFunctions)
  processImports m
  m1 <- runTypeInference m
  ns <- gets compilerNameStore
  let public =
        case xs of
          ["*"] ->
            ns
          names ->
            Environment.restrict names ns
  insertGlobalNamesC (principalPath p) public
  assumptions <- gets (filter (not . isFoldAssumption) . nub . compilerAssumptions)
  forM_ assumptions $
    \Assumption{..} -> do
      tellErrors [NameNotInScope assumptionName (ErrorLocation (principalPath p) assumptionMetadata)]
  unless (null assumptions) $
    throwError NoSuchIdentifier
  pure m1

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
