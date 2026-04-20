{-# LANGUAGE LambdaCase #-}

module Coal.Language.AST.Flattening (
  flattenApplications,
  flattenLambdas,
  flattenApplicationsDeep,
  flattenLambdasDeep,
) where

import Coal.Language.Expression (Expression (..))
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transform)

{-# INLINE flattenApplicationsDeep #-}
flattenApplicationsDeep :: (Data a, Data s, Data t) => Expression a s t -> Expression a s t
flattenApplicationsDeep = transform flattenApplications

{-# INLINE flattenLambdasDeep #-}
flattenLambdasDeep :: (Data a, Data s, Data t) => Expression a s t -> Expression a s t
flattenLambdasDeep = transform flattenLambdas

flattenApplications :: Expression a s t -> Expression a s t
flattenApplications =
  \case
    EApplication a t (EApplication _ _ e e1) e2 ->
      EApplication a t e (e1 <> e2)
    expr ->
      expr

flattenLambdas :: Expression a s t -> Expression a s t
flattenLambdas =
  \case
    ELambda a ps (ELambda _ qs e) ->
      ELambda a (ps <> qs) e
    expr ->
      expr
