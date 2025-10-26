{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TypePhase.TopLevelUnfolds (passTopLevelUnfolds) where

import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Pass
import Coal.Compiler.Stack
import Coal.Compiler.Transform.Expression
import Coal.Compiler.Transform.Flattening (flattenApplication)
import Coal.Language (Expression (..), Kind (..), Pattern (..))
import Coal.Language.Module
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transform)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Extras (Dictionary, Name)

passTopLevelUnfolds :: (Monad m, Monoid a, Data a) => Pass a m (Module a Kind ()) (Module a Kind ())
passTopLevelUnfolds =
  Pass
    { passName = "TopLevelUnfolds"
    , runPass = pass
    }

pass :: (Monad m, Monoid a, Data a) => Module a Kind () -> CompilerT a m (Module a Kind ())
pass = overModuleDefinitionsM (traverse compileTopLevelUnfolds)

compileTopLevelUnfolds :: (Monoid a, Data a, Monad m) => Definition a Kind () -> CompilerT a m (Definition a Kind ())
compileTopLevelUnfolds =
  \case
    DUnfold loc name (UnfoldDef with ps d _) -> do
      e1 <- expandTopLevelUnfold loc ps d
      pure $ DUnfold loc name (UnfoldDef with ps d (Just e1))
    o ->
      pure o

translateFields :: (Monoid a, Monad m) => Name -> (Name, Expression a ()) -> CompilerT a m (Name, Expression a ())
translateFields var (name, e)
  | "@" `Text.isPrefixOf` name =
      pure
        ( "$_" <> Text.drop 1 name
        , lambdaAnyE $ applicationE (varE var) exprs
        )
  | otherwise =
      pure ("$_" <> name, lambdaAnyE e)
 where
  exprs =
    case e of
      ETuple _ _ es ->
        es
      e1 ->
        e1 :| []

expandTopLevelUnfold :: (Monoid a, Data a, Monad m) => a -> NonEmpty (Pattern a ()) -> Dictionary (Expression a ()) -> CompilerT a m (Expression a ())
expandTopLevelUnfold loc ps d = do
  name <- supplied (freshName "unfold")
  d1 <- mapM (translateFields name) (Map.toList d)
  pure $
    transform flattenApplication $
      letE
        name
        ( lambdaE
            ps
            ( ECodataRecord
                loc
                ()
                (Map.fromList d1)
            )
        )
        (varE name)
