{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.PatternMatching (TypeProxy (..), compileEnvelope, compileMatchExprs) where

import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformBiM, transformM)
import Lang.Common.List1 (List1, NonEmpty (..), fromList1)
import Lang.Common.Supply (suppliedName)
import Lang.Label (Label (..))
import Lang.Utils (Dictionary)
import Noll.Compiler.PatternMatching.Compiler (TypeProxy (..), compileEnvelope)
import Noll.Compiler.PatternMatching.Envelope (EnvelopeExpression (..), EnvelopePattern (..))
import Noll.Compiler.PatternMatching.Equation (patternEquation)
import Noll.Compiler.PatternMatching.Rule (MatchMonad (..), matchPatterns)
import Noll.Compiler.Transform.Tree (replaceWith)
import Noll.Language (Binding (..), Choice (..), Clause (..), Expression (..), Pattern (..), Primitive (..))
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..))
import TextShow

class MatchExpressionContext a where
  compileMatchExprs :: a -> MatchMonad a

instance (MatchExpressionContext a) => MatchExpressionContext [a] where
  compileMatchExprs = traverse compileMatchExprs

instance (MatchExpressionContext a) => MatchExpressionContext (Maybe a) where
  compileMatchExprs = traverse compileMatchExprs

instance (MatchExpressionContext a) => MatchExpressionContext (List1 a) where
  compileMatchExprs = traverse compileMatchExprs

instance (MatchExpressionContext a) => MatchExpressionContext (Dictionary a) where
  compileMatchExprs = traverse compileMatchExprs

type MatchClasses a t = (Show a, Data a, Monoid a, Show t, Data t, TypeProxy t, Ord t)

instance (MatchClasses a t, Data k, Ord k) => MatchExpressionContext (Module a k t) where
  compileMatchExprs = transformBiM (compileMatchExprsE :: Expression a t -> MatchMonad (Expression a t))

instance (MatchClasses a t, Data k, Ord k) => MatchExpressionContext (Definition a k t) where
  compileMatchExprs = transformBiM (compileMatchExprsE :: Expression a t -> MatchMonad (Expression a t))

instance (MatchClasses a t) => MatchExpressionContext (Function Expression a t) where
  compileMatchExprs = transformBiM (compileMatchExprsE :: Expression a t -> MatchMonad (Expression a t))

instance (MatchClasses a t) => MatchExpressionContext (Constant Expression a t) where
  compileMatchExprs = transformBiM (compileMatchExprsE :: Expression a t -> MatchMonad (Expression a t))

instance (MatchClasses a t) => MatchExpressionContext (Clause a t) where
  compileMatchExprs = transformBiM (compileMatchExprsE :: Expression a t -> MatchMonad (Expression a t))

instance (MatchClasses a t) => MatchExpressionContext (Binding Expression a t) where
  compileMatchExprs = transformBiM (compileMatchExprsE :: Expression a t -> MatchMonad (Expression a t))

instance (MatchClasses a t) => MatchExpressionContext (Expression a t) where
  compileMatchExprs = transformM compileMatchExprsE

compileMatchExprsE :: (MatchClasses a t) => Expression a t -> MatchMonad (Expression a t)
compileMatchExprsE =
  \case
    EMatch _ _ e cs -> do
      name <- suppliedName
      replaceWith name e <$> compileClauses (Label (expressionType e) name) cs
    e ->
      pure e

compileClauses :: (MatchClasses a t) => Label t -> List1 (Clause a t) -> MatchMonad (Expression a t)
compileClauses ll cs = compileEnvelope <$> matchPatterns [ll] eqs MFail
 where
  eqs = uncurry patternEquation . translateClause <$> fromList1 cs

type TranslatedClause e a t = ([EnvelopePattern (e a) t], EnvelopeExpression (e a) t)

translateClause :: (MatchClasses a t) => Clause a t -> TranslatedClause Expression a t
translateClause (EClause _ p (CPlain _ _ e :| [])) =
  ([translatePattern p], MExpression e)
translateClause _ =
  error "TODO"

translatePattern :: (MatchClasses a t) => Pattern a t -> EnvelopePattern (Expression a) t
translatePattern =
  \case
    PVariable _ ll ->
      MVariable ll
    PConstructor _ ll ps ->
      MConstructor ll (translatePattern <$> ps)
    p@(PLiteral _ LUnit) ->
      MVariable (Label (patternType p) "_")
    p@(PLiteral a prim) ->
      MLiteral (patternType p) (ELiteral a prim)
    PAnnotation _ _ p ->
      translatePattern p
    PAny _ t ->
      MVariable (Label t "_")
    PRecord{} ->
      error "TODO"
    PListCons a t p1 p2 ->
      translatePattern (PConstructor a (Label t "$Cons") [p1, p2])
    PListLiteral a t ps ->
      translatePattern (translateListLiteral a t ps)
    PTuple a t (p :| ps) ->
      translatePattern (PConstructor a (Label t ("$Tuple" <> showt (length ps + 1))) (p : ps))
    POr{} ->
      error "TODO"
    PShorthand{} ->
      error "TODO"
    PAtVariable{} ->
      error "TODO"

translateListLiteral :: (MatchClasses a t) => a -> t -> [Pattern a t] -> Pattern a t
translateListLiteral a t [] = PConstructor a (Label t "$Nil") []
translateListLiteral a t (p : ps) = PConstructor a (Label t "$Cons") [p, translateListLiteral a t ps]
