{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Stack (
  CompilerT (..),
  CompilerEnvironment (..),
  CompilerJournal (..),
  CompilerError (..),
  CompilerFailureMode (..),
  CompilerStack,
  CompilerState (..),
  CompilerConstraint,
  CompilerAssumption,
  ErrorLocation (..),
  runCompilerT,
  evalCompilerT,
  --  insertNameC,
  --  insertNamesC,
  --  setNamesC,
  --  insertConstraintsC,
  --  insertAssumptionsC,
  --  clearAssumptionsC,
  --  clearNameStoreC,
  --  clearConstraintsC,
  --  clearTypeAnnotationParamsC,
  --  setSubstitutionC,
  updateSupply,
--  updateSupplyC,
--  insertSupplyC,
  setVerbatimSourceC,
  setVerbatimSourceForC,
  getVerbatimSourceC,
--  compilerReportConstraintsGenErrors,
--  compilerReportSolverRuleViolations,
  --  compilerSetTypeAnnotationParams,
  setCompilerCurrentModuleC,
  setConfigExecutableNameC,
  setConfigGenerateDotFilesC,
  setConfigGenerateLLVMOutputC,
  setConfigC,
  insertModuleC,
  insertCurrentModuleC,
  getCurrentBuildC,
  updateCurrentBuildC,
  updateBuildC,
  withCurrentModuleC_,
  withCurrentModuleC,
  setBitcodeC,
  insertFreshModule,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Common.Supply (Supply (..))
import Coal.Compiler.Build (ModuleBuild, setBitcode)
import Coal.Compiler.Config
import Coal.Compiler.Environment (CompilerEnvironment (..))
import Coal.Compiler.Error
import Coal.Compiler.Journal (CompilerJournal (..))
import Coal.Compiler.State
import Coal.Language (TypeIndex)
import Coal.Language.Module (Module (..), modulePathName, principalPath)
import Coal.Language.Module.Definition (Path (..))
import Control.Monad.Catch
import Control.Monad.Except (ExceptT (..), MonadError, MonadIO, runExceptT)
import Control.Monad.RWS (RWST, runRWST)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState, gets, modify)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Writer (MonadWriter)
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Extras (Name, fromMaybe)

type CompilerStack a m c = ExceptT CompilerFailureMode (RWST (CompilerEnvironment a) (CompilerJournal a) (CompilerState a) m) c

newtype CompilerT a m c = Compiler {compilerStack :: CompilerStack a m c}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (CompilerEnvironment a)
    , MonadWriter (CompilerJournal a)
    , MonadState (CompilerState a)
    , MonadError CompilerFailureMode
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

instance MonadTrans (CompilerT a) where
  lift = Compiler . lift . lift

runCompilerT :: (Monad m) => CompilerEnvironment a -> CompilerT a m c -> m (Either CompilerFailureMode c, CompilerState a, [CompilerError a])
runCompilerT env com = do
  (c, s, w) <- runRWST (runExceptT (compilerStack com)) env initialCompilerState
  pure (c, s, compilerJournalErrors w)

evalCompilerT :: (Monad m) => CompilerEnvironment a -> CompilerT a m c -> m (Either CompilerFailureMode c)
evalCompilerT env com = do
  (c, _, _) <- runCompilerT env com
  pure c

-- compilerSetTypeAnnotationParams :: (Monad m) => Dictionary (a, TypeIndex Kind) -> CompilerT a m ()
-- compilerSetTypeAnnotationParams params = modify (overCompilerTypeAnnotationParams (const params))

--compilerReportConstraintsGenErrors :: (Monad m) => [ConstraintsGenError a] -> CompilerT a m ()
--compilerReportConstraintsGenErrors errors = modify (overCompilerStateConstraintsGenErrors (<> errors))
--
--compilerReportSolverRuleViolations :: (Monad m) => [InferenceRule Kind a] -> CompilerT a m ()
--compilerReportSolverRuleViolations errors = modify (overCompilerSolverRuleViolations (<> errors))

-- insertSupplyC :: (Monad m) => Int -> CompilerT a m ()
-- insertSupplyC = modify . overCompilerSupply . const

-- insertNameC :: (Monad m) => Name -> IndexedScheme -> CompilerT a m ()
-- insertNameC name scheme_ = modify (overCompilerNameStore (Environment.insert name scheme_))
--
-- insertNamesC :: (Monad m) => [(Name, IndexedScheme)] -> CompilerT a m ()
-- insertNamesC names = modify (overCompilerNameStore (Environment.insertMultiple names))
--
-- setNamesC :: (Monad m) => Environment IndexedScheme -> CompilerT a m ()
-- setNamesC names = modify (overCompilerNameStore (const names))

-- insertConstraintsC :: (Monad m) => [CompilerConstraint a] -> CompilerT a m ()
-- insertConstraintsC cs = modify (overCompilerConstraints (<> cs))
--
-- clearConstraintsC :: (Monad m) => CompilerT a m ()
-- clearConstraintsC = modify (overCompilerConstraints (const mempty))

-- clearTypeAnnotationParamsC :: (Monad m) => CompilerT a m ()
-- clearTypeAnnotationParamsC = modify (overCompilerTypeAnnotationParams (const mempty))

-- clearAssumptionsC :: (Monad m) => CompilerT a m ()
-- clearAssumptionsC = modify (overCompilerAssumptions (const mempty))

-- clearNameStoreC :: (Monad m) => CompilerT a m ()
-- clearNameStoreC = modify (overCompilerNameStore (const mempty))

-- insertAssumptionsC :: (Monad m) => [CompilerAssumption a] -> CompilerT a m ()
-- insertAssumptionsC as = modify (overCompilerAssumptions (<> as))

-- updateSupplyC :: (Monad m) => Int -> CompilerT a m ()
-- updateSupplyC supply = modify (overCompilerSupply (const supply))

-- setSubstitutionC :: (Monad m) => Substitution -> CompilerT a m ()
-- setSubstitutionC sub = modify (overCompilerSubstitution (const sub))

setVerbatimSourceC :: (Monad m) => Name -> Text -> CompilerT a m ()
setVerbatimSourceC name src = modify (overCompilerVerbatimSource (Environment.insert name src))

setVerbatimSourceForC :: (Monad m) => Module a k t -> Text -> CompilerT a m ()
setVerbatimSourceForC module_ = setVerbatimSourceC (modulePathName module_)

getVerbatimSourceC :: (Monad m) => Name -> CompilerT a m Text
getVerbatimSourceC name = do
  s <- gets compilerVerbatimSource
  pure (fromMaybe (error "Implementation error") (Environment.lookup name s))

setCompilerCurrentModuleC :: (Monad m) => Path -> CompilerT a m ()
setCompilerCurrentModuleC path = modify (overCompilerCurrentModule (const path))

setConfigExecutableNameC :: (Monad m) => FilePath -> CompilerT a m ()
setConfigExecutableNameC name = modify (overCompilerConfig (setConfigExecutableName name))

setConfigGenerateDotFilesC :: (Monad m) => Bool -> CompilerT a m ()
setConfigGenerateDotFilesC flag = modify (overCompilerConfig (setConfigGenerateDotFiles flag))

setConfigGenerateLLVMOutputC :: (Monad m) => Bool -> CompilerT a m ()
setConfigGenerateLLVMOutputC flag = modify (overCompilerConfig (setConfigGenerateLLVMOutput flag))

setConfigC :: (Monad m) => CompilerConfig -> CompilerT a m ()
setConfigC config = modify (overCompilerConfig (const config))

insertModuleC :: (Monad m) => Name -> ModuleBuild a -> CompilerT a m ()
insertModuleC name build = modify (overCompilerModules (Environment.insert name build))

insertCurrentModuleC :: (Monad m) => ModuleBuild a -> CompilerT a m ()
insertCurrentModuleC build = do
  path <- gets (principalPath . compilerCurrentModule)
  modify (overCompilerModules (Environment.insert path build))

getBuildC :: (Monad m) => Name -> CompilerT a m (Maybe (ModuleBuild a))
getBuildC path = do
  modules <- gets compilerModules
  pure (Environment.lookup path modules)

getCurrentBuildC :: (Monad m) => CompilerT a m (ModuleBuild a)
getCurrentBuildC = do
  path <- gets (principalPath . compilerCurrentModule)
  fromMaybe (error "Implementation error") <$> getBuildC path

updateBuildC :: (Monad m) => Name -> (ModuleBuild a -> CompilerT a m (ModuleBuild a)) -> CompilerT a m ()
updateBuildC path f = do
  build <- getBuildC path
  case build of
    Nothing ->
      pure ()
    Just b -> do
      updatedBuild <- f b
      modify (overCompilerModules (Environment.insert path updatedBuild))

updateCurrentBuildC :: (Monad m) => (ModuleBuild a -> CompilerT a m (ModuleBuild a)) -> CompilerT a m ()
updateCurrentBuildC f = do
  path <- gets (principalPath . compilerCurrentModule)
  updateBuildC path f

withCurrentModuleC :: (Monad m) => (Module a k t -> CompilerT a m (Module a k t)) -> Module a k t -> CompilerT a m (Module a k t)
withCurrentModuleC f m@(Module p _ _) = do
  setCompilerCurrentModuleC p
  f m

withCurrentModuleC_ :: (Monad m) => (Module a k t -> CompilerT a m ()) -> Module a k t -> CompilerT a m (Module a k t)
withCurrentModuleC_ f m@(Module p _ _) = do
  setCompilerCurrentModuleC p
  f m
  pure m

setBitcodeC :: (Monad m) => Name -> ByteString -> CompilerT a m ()
setBitcodeC name bs = modify (overCompilerModules fn)
 where
  fn (Environment env) = Environment (Map.update (Just . setBitcode bs) name env)

insertFreshModule :: (Monad m) => Name -> CompilerT a m ()
insertFreshModule path = modify (overCompilerFreshModules (Set.insert path))
