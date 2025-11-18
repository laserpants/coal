{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.TypePhase.TypeInference (passTypeInference) where

import Coal.Compiler.Builtin.Definitions (builtinFunctions)
import Coal.Compiler.Environment
import Coal.Compiler.Journal
import Coal.Compiler.Module.Builders
import Coal.Compiler.Module.Bundle
import Coal.Compiler.Pass
import Coal.Compiler.Stack
import Coal.Compiler.TypeInference (typeDefinitionsC)
import Coal.Language
import Coal.Language.Module
import Coal.TypeSystem.Constraint.Assumption (Assumption (..))
import Coal.TypeSystem.Substitution
import Control.Monad.Except
import Control.Monad.Reader (local)
import Control.Monad.State
import Data.Data (Data)
import Data.List (nub)
import qualified Data.Text as Text

passTypeInference :: (MonadIO m, Monoid a, Data a, Eq a, Show a) => Pass a m (Module a Kind ()) (Module a Kind IndexedType)
passTypeInference =
  Pass
    { passName = "TypeInference"
    , runPass = pass
    }

-- TODO: Maybe look these up in environment and add additional constraints?
isFoldAssumption :: Assumption a t -> Bool
isFoldAssumption Assumption{..} = "!" `Text.isPrefixOf` assumptionName

pass :: (MonadIO m, Monoid a, Data a, Eq a, Show a) => Module a Kind () -> CompilerT a m (Module a Kind IndexedType)
pass m@(Module path _ _) = do
  setCompilerCurrentModuleC path
  clearAssumptionsC
  clearNameStoreC

  bundle@ModuleBundle{..} <- prepareBundle m
  insertModuleC (principalPath path) bundle

  typeConstructors <- evalStateT typeConstructorEnv ModuleBundle{..}
  let cmpEnv =
        CompilerEnvironment
          { compilerDataConstructorEnvironment = moduleDataConstructors
          , compilerTypeConstructorEnvironment = typeConstructors
          , compilerAliasEnvironment = moduleAliases
          , compilerCodataAccessorEnvironment = moduleCodataAccessors
          , compilerTraitEnvironment = moduleTraits
          , compilerInstanceEnvironment = moduleInstances
          , compilerDictionaryNameEnvironment = mempty
          , compilerKernelEnvironment = KernelEnvironment mempty mempty mempty
          }

  env <- buildEnv
  setNamesC env
  insertNamesC builtinFunctions

  m1 <- local (const cmpEnv) (runTypeInference m)
  names <- gets compilerNameStore
  replacePlaceholders names

  assumptions <- gets (filter (not . isFoldAssumption) . nub . compilerAssumptions)
  forM_ assumptions $
    \Assumption{..} -> do
      tellErrors [NameNotInScope assumptionName (ErrorLocation (principalPath path) assumptionMetadata)]
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
