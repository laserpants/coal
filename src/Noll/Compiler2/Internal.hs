{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler2.Internal where

import Control.Monad.RWS (RWST, runRWST)
import Control.Monad.Reader (MonadReader, Reader, ReaderT, ask, asks, runReader, runReaderT)
import Control.Monad.State (MonadState, StateT, gets, modify, put, runState, runStateT)
import Lang.Common.Environment (Environment (..))
import Lang.Common.Supply (Supply (..), supplied)
import Lang.Utils (Dictionary, Name, Over, forM_, (<$$$>))
import Noll.Compiler.Transform.Type.AliasExpansion
import Noll.Language
import Noll.SystemF

data Compiler2Environment o k t = Compiler2Environment
  { compiler2DataConstructorEnv :: Environment (Constructor o k t)
  , compiler2TypeConstructorEnv :: Environment Kind
  , compiler2TraitEnv :: Environment (o k, Environment (Scheme o k t))
  , compiler2AliasEnv :: AliasEnvironment
  }
  deriving (Show, Eq, Ord, Read)

data Compiler2State = Compiler2State
  { compiler2Supply :: Int
  , compiler2NameStore :: Environment (Scheme TypeIndex Kind IndexedType)
  , compilerSubstitution :: Substitution
  }
  deriving (Show, Eq, Ord, Read)

instance Supply Compiler2State where
  updateSupply = overCompiler2Supply
  getSupply = compiler2Supply

{-# INLINE overCompiler2NameStore #-}
overCompiler2NameStore :: Over Compiler2State (Environment (Scheme TypeIndex Kind IndexedType))
overCompiler2NameStore fn Compiler2State{..} = Compiler2State{compiler2NameStore = fn compiler2NameStore, ..}

{-# INLINE overCompiler2Supply #-}
overCompiler2Supply :: Over Compiler2State Int
overCompiler2Supply fn Compiler2State{..} = Compiler2State{compiler2Supply = fn compiler2Supply, ..}

initialCompiler2State :: Compiler2State
initialCompiler2State =
  Compiler2State
    { compiler2Supply = 0
    , compiler2NameStore = mempty
    , compilerSubstitution = mempty
    }

type Compiler2Stack m c = RWST (Compiler2Environment TypeIndex Kind IndexedType) () Compiler2State m c

newtype Compiler2T m c = Compiler2 {compiler2Stack :: Compiler2Stack m c}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (Compiler2Environment TypeIndex Kind IndexedType)
    , MonadState Compiler2State
    )

{-# INLINE runCompiler2T #-}
runCompiler2T :: (Monad m) => Compiler2Environment TypeIndex Kind IndexedType -> Compiler2T m c -> m (c, Compiler2State)
runCompiler2T env com = do
  (c, s, _) <- runRWST (compiler2Stack com) env initialCompiler2State
  pure (c, s)

{-# INLINE evalCompiler2T #-}
evalCompiler2T :: (Monad m) => Compiler2Environment TypeIndex Kind IndexedType -> Compiler2T m c -> m c
evalCompiler2T = fst <$$$> runCompiler2T
