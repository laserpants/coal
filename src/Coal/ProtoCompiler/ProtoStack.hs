{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoStack (
  ProtoCompilerT (..),
  runProtoCompilerT,
  evalProtoCompilerT,
) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.RWS (RWST, runRWST)

type ProtoCompilerStack m o = ExceptT () (RWST () () () m) o

newtype ProtoCompilerT m o = ProtoCompiler {protoOcompilerStack :: ProtoCompilerStack m o}
  deriving
    ( Functor
    , Applicative
    , Monad
    , --    , MonadReader (CompilerEnvironment a)
      --    , MonadWriter (CompilerJournal a)
      --    , MonadState (CompilerState a)
      --    , MonadError CompilerFailureMode
      MonadIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

-- TODO
runProtoCompilerT :: (Monad m) => ProtoCompilerT m o -> m (Either () o, (), [()])
runProtoCompilerT com = do
  (c, s, w) <- runRWST (runExceptT (protoOcompilerStack com)) () ()
  pure (c, s, [])

-- TODO
evalProtoCompilerT :: (Monad m) => ProtoCompilerT m o -> m (Either () o)
evalProtoCompilerT com = do
  (c, _, _) <- runProtoCompilerT com
  pure c
