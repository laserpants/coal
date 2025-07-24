{-# LANGUAGE LambdaCase #-}

module Noll.Compiler2.Transform (
  flattenApplication,
  flattenLambda,
) where

import Noll.Language.Expression (Expression (..))

flattenApplication :: Expression a t -> Expression a t
flattenApplication =
  \case
    EApplication a t (EApplication _ _ e a1) a2 ->
      EApplication a t e (a1 <> a2)
    expr ->
      expr

flattenLambda :: Expression a t -> Expression a t
flattenLambda =
  \case
    ELambda a ps (ELambda _ qs e) ->
      ELambda a (ps <> qs) e
    expr ->
      expr
