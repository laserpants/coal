{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Transform.Definition.Unfold where

import Coal.Common.Supply (suppliedName)
import Coal.Compiler.Transform.Expression
import Coal.Compiler.Transform.Flattening (flattenApplication)
import Coal.Language (Expression (..), Kind (..), Pattern (..))
import Coal.Language.Module (Definition (..), UnfoldDef (..))
import Control.Monad.RWS (RWS, runRWS)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transform)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Extra (Dictionary, Name)

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
      e1 <- expandTopLevelUnfold ps d
      pure $ DUnfold loc name (UnfoldDef with ps d (Just e1))
    o ->
      pure o

translateFields :: (Monoid a) => Name -> (Name, Expression a ()) -> UnfoldTopLevelUnfolds (Name, Expression a ())
translateFields var (name, e)
  | "@" `Text.isPrefixOf` name =
      pure
        ( "$_" <> Text.drop 1 name
        , lambdaAnyE $
            applicationE
              (varE var)
              (e :| [])
        )
  | otherwise =
      pure ("$_" <> name, lambdaAnyE e)

expandTopLevelUnfold :: (Monoid a, Data a) => NonEmpty (Pattern a ()) -> Dictionary (Expression a ()) -> UnfoldTopLevelUnfolds (Expression a ())
expandTopLevelUnfold ps d = do
  name <- suppliedName
  d1 <- mapM (translateFields name) (Map.toList d)
  pure $
    transform flattenApplication $
      letE
        name
        ( lambdaE
            ps
            ( ECodataRecord
                mempty
                ()
                (Map.fromList d1)
            )
        )
        (varE name)
