{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Transform.LambdaMatch (
  LambdaMatchExpansion (..),
  CompileLambdaMatchContext (..),
  evalLambdaMatchExpansion,
  runLambdaMatchExpansion,
)
where

import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.Language.Module (ConstantDef (..), Definition (..), FunctionDef (..), Module (..))
import Control.Monad.RWS (RWS, runRWS)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformM)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Extra (Dictionary, Name)

newtype LambdaMatchExpansion a = LambdaMatchExpansion {natExpansionStack :: RWS Name () Int a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Name
    , MonadState Int
    )

evalLambdaMatchExpansion :: Name -> Int -> LambdaMatchExpansion a -> a
evalLambdaMatchExpansion r s = fst . runLambdaMatchExpansion r s

runLambdaMatchExpansion :: Name -> Int -> LambdaMatchExpansion a -> (a, Int)
runLambdaMatchExpansion r s e = (a, s')
 where
  (a, s', _) = runRWS (natExpansionStack e) r s

class CompileLambdaMatchContext a where
  compileLambdaMatch :: a -> LambdaMatchExpansion a

instance (CompileLambdaMatchContext a) => CompileLambdaMatchContext [a] where
  compileLambdaMatch = traverse compileLambdaMatch

instance (CompileLambdaMatchContext a) => CompileLambdaMatchContext (NonEmpty a) where
  compileLambdaMatch = traverse compileLambdaMatch

instance (CompileLambdaMatchContext a) => CompileLambdaMatchContext (Dictionary a) where
  compileLambdaMatch = traverse compileLambdaMatch

instance (Monoid a, Data a) => CompileLambdaMatchContext (Expression a ()) where
  compileLambdaMatch = transformM go
   where
    go =
      \case
        ELambdaMatch a t cs Nothing -> do
          e1 <- expandLambdaMatch a t cs
          pure (ELambdaMatch a t cs (Just e1))
        e ->
          pure e

expandLambdaMatch a t cs =
  pure $
    ELambda
      mempty
      (PVariable mempty (Label () "$lambda_match") :| [])
      ( EMatch
          mempty
          ()
          (EVariable mempty (Label () "$lambda_match"))
          cs
      )

instance (Monoid a, Data a) => CompileLambdaMatchContext (Module a Kind ()) where
  compileLambdaMatch =
    \case
      Module p ns o ->
        Module p ns <$> compileLambdaMatch o

instance (Monoid a, Data a) => CompileLambdaMatchContext (FunctionDef a ()) where
  compileLambdaMatch =
    \case
      FunctionDef a u w ps e ->
        FunctionDef a u w ps <$> compileLambdaMatch e

instance (Monoid a, Data a) => CompileLambdaMatchContext (ConstantDef a ()) where
  compileLambdaMatch =
    \case
      ConstantDef a u w e ->
        ConstantDef a u w <$> compileLambdaMatch e

instance (Monoid a, Data a) => CompileLambdaMatchContext (Definition a Kind ()) where
  compileLambdaMatch =
    \case
      DFunction loc name f fs ->
        DFunction loc name <$> compileLambdaMatch f <*> traverse compileLambdaMatch fs
      DConstant loc name g fs ->
        DConstant loc name <$> compileLambdaMatch g <*> traverse compileLambdaMatch fs
      -- TODO
      o ->
        pure o
