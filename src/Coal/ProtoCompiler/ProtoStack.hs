{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoStack (
  ProtoCompilerT (..),
  runProtoCompilerT,
  evalProtoCompilerT,
  updateSupplyC,
  insertBuildC,
  setCurrentPathC,
  setCurrentModuleC,
  getCurrentBuildC,
) where

import qualified Coal.Common.Environment as Environment
import Coal.Language.Module.Path (Path (..), principalPath)
import Coal.ProtoCompiler.ProtoBuild (ProtoBuild (..))
import Coal.ProtoCompiler.ProtoJournal (ProtoCompilerJournal (..))
import Coal.ProtoCompiler.ProtoState
import Coal.ProtoLanguage.ProtoModule
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT (..), MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.RWS (MonadReader, MonadState, MonadWriter, RWST, runRWST)
import Control.Monad.State (get, modify)

type ProtoCompilerStack m a o = ExceptT () (RWST () (ProtoCompilerJournal a) (ProtoCompilerState a) m) o

newtype ProtoCompilerT m a o = ProtoCompiler {protoOcompilerStack :: ProtoCompilerStack m a o}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader ()
    , MonadWriter (ProtoCompilerJournal a)
    , MonadState (ProtoCompilerState a)
    , MonadError ()
    , --    , MonadReader (CompilerEnvironment a)
      --    , MonadWriter (CompilerJournal a)
      --    , MonadError CompilerFailureMode
      MonadIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

-- TODO
runProtoCompilerT :: (Monad m) => ProtoCompilerT m a o -> m (Either () o, ProtoCompilerState a, [()])
runProtoCompilerT com = do
  (c, s, w) <- runRWST (runExceptT (protoOcompilerStack com)) () initialProtoCompilerState
  pure (c, s, [])

-- TODO
evalProtoCompilerT :: (Monad m) => ProtoCompilerT m a o -> m (Either () o)
evalProtoCompilerT com = do
  (c, _, _) <- runProtoCompilerT com
  pure c

updateSupplyC :: (Monad m) => Int -> ProtoCompilerT m a ()
updateSupplyC supply = modify (overProtoCompilerSupply (const supply))

insertBuildC :: (Monad m) => ProtoBuild a -> ProtoCompilerT m a ()
insertBuildC ProtoBuild{..} = modify (overProtoCompilerModules (Environment.insert name ProtoBuild{..}))
 where
  name = principalPath protoObuildPath

setCurrentPathC :: (Monad m) => Path -> ProtoCompilerT m a ()
setCurrentPathC path = modify (overProtoCompilerCurrentPath (const path))

setCurrentModuleC :: (Monad m) => ProtoModule a s t -> ProtoCompilerT m a ()
setCurrentModuleC ProtoModule{..} = setCurrentPathC protoOmodulePath

getCurrentBuildC :: (Monad m) => ProtoCompilerT m a (ProtoBuild a)
getCurrentBuildC = do
  ProtoCompilerState{..} <- get
  case Environment.lookup (principalPath protoOcompilerCurrentPath) protoOcompilerModules of
    Nothing ->
      error "Implementation error"
    Just build ->
      return build
