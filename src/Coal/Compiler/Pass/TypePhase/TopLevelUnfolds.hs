{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TypePhase.TopLevelUnfolds (passTopLevelUnfolds) where

import Coal.AST.Flattening (deepFlattenApplications)
import Coal.AST.Shorthand (applicationE', lambdaAnyE', lambdaE', letE', varE')
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language (Expression (..), Kind (..), Pattern (..))
import Coal.Language.Module
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Extras (Dictionary, Name)

passTopLevelUnfolds :: (Monad m, Data a) => Pass a m (Module a Kind ()) (Module a Kind ())
passTopLevelUnfolds = Pass{runPass = pass}

pass :: (Monad m, Data a) => Module a Kind () -> CompilerT a m (Module a Kind ())
pass = withCurrentModuleC (overModuleDefinitionsM (traverse compileTopLevelUnfolds))

compileTopLevelUnfolds :: (Data a, Monad m) => Definition a Kind () -> CompilerT a m (Definition a Kind ())
compileTopLevelUnfolds =
  \case
    DUnfold loc name (UnfoldDefinition with ps d _) -> do
      e1 <- expandTopLevelUnfold loc ps d
      pure $ DUnfold loc name (UnfoldDefinition with ps d (Just e1))
    o ->
      pure o

translateFields :: (Monad m) => a -> Name -> (Name, Expression a ()) -> CompilerT a m (Name, Expression a ())
translateFields loc var (name, e)
  | "@" `Text.isPrefixOf` name =
      pure
        ( "$_" <> Text.drop 1 name
        , lambdaAnyE' loc $ applicationE' loc (varE' loc var) exprs
        )
  | otherwise =
      pure ("$_" <> name, lambdaAnyE' loc e)
 where
  exprs =
    case e of
      ETuple _ _ es ->
        es
      e1 ->
        e1 :| []

expandTopLevelUnfold :: (Data a, Monad m) => a -> NonEmpty (Pattern a ()) -> Dictionary (Expression a ()) -> CompilerT a m (Expression a ())
expandTopLevelUnfold loc ps d = do
  name <- supplied (freshName "unfold")
  d1 <- mapM (translateFields loc name) (Map.toList d)
  pure $
    deepFlattenApplications $
      letE'
        loc
        name
        ( lambdaE'
            loc
            ps
            ( ECodataRecord
                loc
                ()
                (Map.fromList d1)
            )
        )
        (varE' loc name)
