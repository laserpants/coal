{-# LANGUAGE LambdaCase #-}

module Noll.Compiler.Transform (
  flattenApplications,
  flattenLambdas,
) where

import Noll.Language.Expression (Expression (..))

flattenApplications :: Expression a t -> Expression a t
flattenApplications =
  \case
    EApplication a t (EApplication _ _ e a1) a2 ->
      EApplication a t e (a1 <> a2)
    expr ->
      expr

flattenLambdas :: Expression a t -> Expression a t
flattenLambdas =
  \case
    ELambda a ps (ELambda _ qs e) ->
      ELambda a (ps <> qs) e
    expr ->
      expr
