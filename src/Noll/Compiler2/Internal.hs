{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler2.Internal where

import Control.Monad.RWS (RWST, runRWST)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState, modify)
import Lang.Common.Environment (Environment (..))
import Lang.Common.Supply (Supply (..))
import Lang.Utils (Name, Over, (<$$$>))
import Noll.Compiler.Transform.Type.AliasExpansion
import Noll.Language
import Noll.SystemF

import qualified Lang.Common.Environment as Environment

data Compiler2Environment o k t = Compiler2Environment
  { compiler2DataConstructorEnv :: Environment (Constructor o k t)
  , compiler2TypeConstructorEnv :: Environment Kind
  , compiler2TraitEnv :: Environment (o k, Environment (Scheme o k t))
  , compiler2AliasEnv :: AliasEnvironment
  }
  deriving (Show, Eq, Ord, Read)

type CompilerConstraint a = Constraint (InferenceRule Kind a) TypeIndex Kind IndexedType

data Compiler2State a = Compiler2State
  { compiler2Supply :: Int
  , compiler2NameStore :: Environment (Scheme TypeIndex Kind IndexedType)
  , compiler2Substitution :: Substitution
  , compiler2Constraints :: [CompilerConstraint a]
  --  , compiler2ConstraintsGenErrors :: [ConstraintsGenError a]
  }
  deriving (Show, Eq, Ord, Read)

instance Supply (Compiler2State a) where
  updateSupply = overCompiler2Supply
  getSupply = compiler2Supply

{-# INLINE overCompiler2NameStore #-}
overCompiler2NameStore :: Over (Compiler2State a) (Environment (Scheme TypeIndex Kind IndexedType))
overCompiler2NameStore fn Compiler2State{..} = Compiler2State{compiler2NameStore = fn compiler2NameStore, ..}

{-# INLINE overCompiler2Supply #-}
overCompiler2Supply :: Over (Compiler2State a) Int
overCompiler2Supply fn Compiler2State{..} = Compiler2State{compiler2Supply = fn compiler2Supply, ..}

{-# INLINE overCompiler2Constraints #-}
overCompiler2Constraints :: Over (Compiler2State a) [CompilerConstraint a]
overCompiler2Constraints fn Compiler2State{..} = Compiler2State{compiler2Constraints = fn compiler2Constraints, ..}

initialCompiler2State :: Compiler2State a
initialCompiler2State =
  Compiler2State
    { compiler2Supply = 0
    , compiler2NameStore = mempty
    , compiler2Substitution = mempty
    , compiler2Constraints = []
    }

type Compiler2Stack a m c = RWST (Compiler2Environment TypeIndex Kind IndexedType) () (Compiler2State a) m c

newtype Compiler2T a m c = Compiler2 {compiler2Stack :: Compiler2Stack a m c}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (Compiler2Environment TypeIndex Kind IndexedType)
    , MonadState (Compiler2State a)
    )

{-# INLINE runCompiler2T #-}
runCompiler2T :: (Monad m) => Compiler2Environment TypeIndex Kind IndexedType -> Compiler2T a m c -> m (c, Compiler2State a)
runCompiler2T env com = do
  (c, s, _) <- runRWST (compiler2Stack com) env initialCompiler2State
  pure (c, s)

{-# INLINE evalCompiler2T #-}
evalCompiler2T :: (Monad m) => Compiler2Environment TypeIndex Kind IndexedType -> Compiler2T a m c -> m c
evalCompiler2T = fst <$$$> runCompiler2T

{-# INLINE insertSupplyC #-}
insertSupplyC :: (Monad m) => Int -> Compiler2T a m ()
insertSupplyC = modify . overCompiler2Supply . const

{-# INLINE insertNamesC #-}
insertNamesC :: (Monad m) => [(Name, Scheme TypeIndex Kind IndexedType)] -> Compiler2T a m ()
insertNamesC names = modify (overCompiler2NameStore (Environment.insertMultiple names))

insertConstraintsC :: (Monad m) => [CompilerConstraint a] -> Compiler2T a m ()
insertConstraintsC cs = modify (overCompiler2Constraints (<> cs))
