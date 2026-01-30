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
deepFlattenApplications :: (Data a, Data s, Data t) => Expression a s t -> Expression a s t
deepFlattenApplications = transform flattenApplication

{-# INLINE deepFlattenLambdas #-}
deepFlattenLambdas :: (Data a, Data s, Data t) => Expression a s t -> Expression a s t
deepFlattenLambdas = transform flattenLambda

flattenApplication :: Expression a s t -> Expression a s t
flattenApplication =
  \case
    EApplication a t (EApplication _ _ e e1) e2 ->
      EApplication a t e (e1 <> e2)
    expr ->
      expr

flattenLambda :: Expression a s t -> Expression a s t
flattenLambda =
  \case
    ELambda a ps (ELambda _ qs e) ->
      ELambda a (ps <> qs) e
    expr ->
      expr
