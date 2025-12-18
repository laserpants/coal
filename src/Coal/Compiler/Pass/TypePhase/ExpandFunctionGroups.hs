{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TypePhase.ExpandFunctionGroups (passExpandFunctionGroups) where

import Coal.AST.Metadata (Metadata (..))
import Coal.AST.Shorthand (matchE, tupleE, tupleP, varE, varP)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT, withCurrentModuleC)
import Coal.Language (Choice (..), Clause (..), Expression (..), Kind (..))
import Coal.Language.Module
import Coal.Language.Trait (With (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import TextShow (showt)

passExpandFunctionGroups :: (Monad m) => Pass Metadata m (Module Metadata Kind ()) (Module Metadata Kind ())
passExpandFunctionGroups = Pass{runPass = pass}

pass :: (Monad m) => Module Metadata Kind () -> CompilerT Metadata m (Module Metadata Kind ())
pass = withCurrentModuleC expandFunctionGroups

class FunctionGroupContext e where
  expandFunctionGroups :: (Monad m) => e -> CompilerT Metadata m e

instance (FunctionGroupContext e) => FunctionGroupContext [e] where
  expandFunctionGroups = traverse expandFunctionGroups

instance (FunctionGroupContext e) => FunctionGroupContext (NonEmpty e) where
  expandFunctionGroups = traverse expandFunctionGroups

instance FunctionGroupContext (Module Metadata Kind ()) where
  expandFunctionGroups =
    \case
      Module p ns o -> do
        clauses <- traverse expandGroups o
        pure (Module p ns (concat clauses))

expandGroups :: (Monad m) => Definition Metadata Kind () -> CompilerT Metadata m [Definition Metadata Kind ()]
expandGroups =
  \case
    d@(DFunction _ _ (_ :| []) _) ->
      pure [d]
    DFunction loc name fs@(FunctionDefinition _ w _ ps _ :| _) gs ->
      pure [DConstant loc name e1 gs]
     where
      e1 = ConstantDefinition loc w (With [] ()) (toExpr (length ps) loc (NonEmpty.toList fs))
    DInstance loc name (InstanceDefinition ps t ds) -> do
      ds' <- traverse expandGroups ds
      pure [DInstance loc name (InstanceDefinition ps t (concat ds'))]
    d ->
      pure [d]

toExpr :: Int -> Metadata -> [FunctionDefinition Metadata ()] -> Expression Metadata ()
toExpr n loc fs = ELambda loc (varP <$> args) (matchE (var args) clauses)
 where
  ns = NonEmpty.fromList [1 .. n]
  args = (<>) "$arg_" . showt <$> ns
  clauses =
    case [EClause a (pat ps) (CPlain mempty [] e :| []) | FunctionDefinition a _ _ ps e <- fs] of
      c : cs ->
        c :| cs
      [] ->
        error "Implementation error"
  pat ps
    | length ps == 1 = NonEmpty.head ps
    | otherwise = tupleP ps
  var qs
    | length qs == 1 = varE (NonEmpty.head qs)
    | otherwise = tupleE (varE <$> qs)
