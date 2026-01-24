{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoStack where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.RWS (RWST)

type ProtoCompilerStack m o = ExceptT () (RWST () () () m) o

newtype ProtoCompilerT m o = Compiler {protoOcompilerStack :: ProtoCompilerStack m o}
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
