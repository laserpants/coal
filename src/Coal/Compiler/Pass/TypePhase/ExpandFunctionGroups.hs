{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TypePhase.ExpandFunctionGroups (passExpandFunctionGroups) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Pass
import Coal.Compiler.Stack
import Coal.Compiler.Transform.Expression
import Coal.Language (Choice (..), Clause (..), Expression (..), Kind (..))
import Coal.Language.Module
import Coal.Language.Trait (With (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import TextShow (showt)

passExpandFunctionGroups :: (Monad m) => Pass Metadata m (Module Metadata Kind ()) (Module Metadata Kind ())
passExpandFunctionGroups =
  Pass
    { passName = "ExpandFunctionGroups"
    , runPass = pass
    }

pass :: (Monad m) => Module Metadata Kind () -> CompilerT Metadata m (Module Metadata Kind ())
pass m@(Module p _ _) = do
  setCompilerModuleC p
  expandFunctionGroups m

class RuleContext e where
  expandFunctionGroups :: (Monad m) => e -> CompilerT Metadata m e

instance (RuleContext e) => RuleContext [e] where
  expandFunctionGroups = traverse expandFunctionGroups

instance (RuleContext e) => RuleContext (NonEmpty e) where
  expandFunctionGroups = traverse expandFunctionGroups

instance RuleContext (Module Metadata Kind ()) where
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
    DFunction loc name fs@(FunctionDef _ w _ ps _ :| _) gs ->
      pure [DConstant loc name e1 gs]
     where
      e1 = ConstantDef loc w (With [] ()) (toExpr (NonEmpty.length ps) loc (NonEmpty.toList fs))
    d ->
      pure [d]

toExpr :: Int -> Metadata -> [FunctionDef Metadata ()] -> Expression Metadata ()
toExpr n loc fs =
  ELambda
    loc
    (varP <$> qs)
    (matchE (tupleE (varE <$> qs)) clauses)
 where
  ns = NonEmpty.fromList [1 .. n]
  qs = (<>) "$arg_" . showt <$> ns
  clauses =
    case [EClause a (tupleP ps) (CPlain mempty [] e :| []) | FunctionDef a _ _ ps e <- fs] of
      c : cs -> c :| cs
      [] -> error "Implementation error"
