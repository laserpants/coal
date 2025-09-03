{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Transform.Definition.Unfold where

import Coal.Common.Label (Label (..), labelName)
import Coal.Common.Supply (suppliedName)
import Coal.Compiler.Transform.Expression
import Coal.Compiler.Transform.Flattening (flattenApplication)
import Coal.Compiler.Transform.Tree (replace)
import Coal.Language (Choice (..), Clause (..), Expression (..), Kind (..), Pattern (..))
import Coal.Language.Module (ConstantDef (..), Definition (..), FunctionDef (..), Module (..), UnfoldDef (..))
import Control.Monad.RWS (RWS, runRWS)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (execWriter, tell)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transform, transformM)
import Data.List.NonEmpty (NonEmpty (..))
import Extra (Dictionary, Name, const2)

import qualified Data.Map.Strict as Map

newtype UnfoldTopLevelUnfolds a = UnfoldTopLevelUnfolds {unfoldExpansionStack :: RWS Name () Int a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Name
    , MonadState Int
    )

runTopLevelUnfolds :: Name -> Int -> UnfoldTopLevelUnfolds a -> (a, Int)
runTopLevelUnfolds r s e = (a, s')
 where
  (a, s', _) = runRWS (unfoldExpansionStack e) r s

compileTopLevelUnfolds :: (Monoid a, Data a) => Definition a Kind () -> UnfoldTopLevelUnfolds (Definition a Kind ())
compileTopLevelUnfolds =
  \case
    DUnfold loc name (UnfoldDef with ps d _) -> do
      e1 <- expandTopLevelUnfold name ps d
      pure $ DUnfold loc name (UnfoldDef with ps d (Just e1))
    o ->
      pure o

renameRecursiveCall :: (Monoid a, Data a) => Name -> Name -> Expression a () -> Expression a ()
renameRecursiveCall old new = replace old (const2 $ varE new)

expandTopLevelUnfold :: (Monoid a, Data a) => Name -> NonEmpty (Pattern a ()) -> Dictionary (Expression a ()) -> UnfoldTopLevelUnfolds (Expression a ())
expandTopLevelUnfold var ps d = do
  name <- suppliedName
  pure $
    transform flattenApplication $
      letE
        name
        ( lambdaE
            ps
            ( ECodataFields
                mempty
                ()
                (Map.mapKeys ("$_" <>) (Map.map (lambdaAnyE . renameRecursiveCall var name) d))
            )
        )
        (varE name)
