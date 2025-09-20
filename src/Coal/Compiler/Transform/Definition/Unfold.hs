{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Transform.Definition.Unfold (compileTopLevelUnfolds) where

import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Stack
import Coal.Compiler.Transform.Expression
import Coal.Compiler.Transform.Flattening (flattenApplication)
import Coal.Language (Expression (..), Kind (..), Pattern (..))
import Coal.Language.Module.Definition
import Coal.Language.Module.Definition.Unfold
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transform)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Extra (Dictionary, Name)

compileTopLevelUnfolds :: (Monoid a, Data a, Monad m) => Definition a Kind () -> CompilerT a m (Definition a Kind ())
compileTopLevelUnfolds =
  \case
    DUnfold loc name (UnfoldDef with ps d _) -> do
      e1 <- expandTopLevelUnfold ps d
      pure $ DUnfold loc name (UnfoldDef with ps d (Just e1))
    o ->
      pure o

translateFields :: (Monoid a, Monad m) => Name -> (Name, Expression a ()) -> CompilerT a m (Name, Expression a ())
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

expandTopLevelUnfold :: (Monoid a, Data a, Monad m) => NonEmpty (Pattern a ()) -> Dictionary (Expression a ()) -> CompilerT a m (Expression a ())
expandTopLevelUnfold ps d = do
  name <- supplied (freshName "unfold")
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
