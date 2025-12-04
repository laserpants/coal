{-# LANGUAGE LambdaCase #-}

module Coal.AST.Flattening (
  flattenApplication,
  flattenLambda,
  deepFlattenApplications,
  deepFlattenLambdas,
) where

import Coal.Language.Expression (Expression (..))
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transform)

{-# INLINE deepFlattenApplications #-}
deepFlattenApplications :: (Data a, Data t) => Expression a t -> Expression a t
deepFlattenApplications = transform flattenApplication

{-# INLINE deepFlattenLambdas #-}
deepFlattenLambdas :: (Data a, Data t) => Expression a t -> Expression a t
deepFlattenLambdas = transform flattenLambda

flattenApplication :: Expression a t -> Expression a t
flattenApplication =
  \case
    EApplication a t (EApplication _ _ e e1) e2 ->
      EApplication a t e (e1 <> e2)
    expr ->
      expr

flattenLambda :: Expression a t -> Expression a t
flattenLambda =
  \case
    ELambda a ps (ELambda _ qs e) ->
      ELambda a (ps <> qs) e
    expr ->
      expr
