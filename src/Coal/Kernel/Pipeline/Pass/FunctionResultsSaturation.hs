{-# LANGUAGE OverloadedStrings #-}

{- |
Normalization pass 7: Function results saturation.

Eta-expands every top-level function whose result type is itself a function
type, adding fresh parameters until the result is non-functional. This ensures
that all functions return simple values rather than closures.

= Invariant established

After this pass, no top-level 'DFunction' has a function type as its result
type. All functions return data values, primitives, or control-flow
constructors.

= Transformation

For a function @f(v₁, …, vₙ) = e@ where @typeOf(e)@ is a function type
@A₁ → … → Aₖ → R@:

  1. Generate @k@ fresh parameter labels @r₁ : A₁, …, rₖ : Aₖ@.
  2. Rewrite to @f(v₁, …, vₙ, r₁, …, rₖ) = e(r₁, …, rₖ)@.

The process is repeated until @typeOf(body)@ is not a function type.

= Preconditions

Passes 3 (lambda flattening), 5 (lambda lifting), and 6 (top-level function
normalization) have already run.
-}
module Coal.Kernel.Pipeline.Pass.FunctionResultsSaturation (
  functionResultsSaturation,
) where

import qualified Data.List.NonEmpty as NonEmpty

import Coal.Kernel.Language.Expr (Expr (..), Label (..))
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Language.Type.Function (isFunction)
import Coal.Kernel.Language.Type.HasType (typeOf, unfoldType)
import Coal.Kernel.Pipeline (Pass, PipelineT, freshName)

{- | Eta-expand every top-level function whose result type is itself a
function type, adding fresh parameters until the result is non-functional.

For a function @f(v₁, …, vₙ) = e@ where @typeOf(e)@ is a function type
@A₁ → … → Aₖ → R@:

  1. Generate @k@ fresh parameter labels @r₁ : A₁, …, rₖ : Aₖ@.
  2. Rewrite to @f(v₁, …, vₙ, r₁, …, rₖ) = e(r₁, …, rₖ)@.

The process is repeated until @typeOf(body)@ is not a function type.

__Preconditions__: passes 3 (lambda flattening), 5 (lambda lifting), and 6
(top-level function normalization) have already run.
-}
functionResultsSaturation :: (Monad m) => Pass m (Module Type) (Module Type)
functionResultsSaturation m = do
  objs' <- mapM saturateObject (moduleObjects m)
  pure m{moduleObjects = objs'}

-- --------------------------------------------------------------------------
-- Object
-- --------------------------------------------------------------------------

saturateObject :: (Monad m) => Object Type -> PipelineT m (Object Type)
saturateObject obj =
  case obj of
    DFunction name params body ->
      if isFunction body
        then do
          (extraParams, body') <- etaExpand body
          pure (DFunction name (params ++ extraParams) body')
        else pure obj
    DConstant name body ->
      if isFunction body
        then do
          (extraParams, body') <- etaExpand body
          pure (DFunction name extraParams body')
        else pure obj
    DExternal{} ->
      pure obj
    DData{} ->
      pure obj

-- --------------------------------------------------------------------------
-- Eta-expansion
-- --------------------------------------------------------------------------

{- | Given an expression @e@ whose type is a function type, produce fresh
parameter labels and a new body @e(r₁, …, rₖ)@. Repeats until the result
type is not a function type.
-}
etaExpand :: (Monad m) => Expr Type -> PipelineT m ([Label Type], Expr Type)
etaExpand body = do
  -- The argument types are all-but-last elements of unfoldType(typeOf(body)).
  let bodyType = typeOf body
      -- unfoldType gives: [A1, A2, ..., Ak, R] for A1 -> A2 -> ... -> Ak -> R
      typeComponents = NonEmpty.toList (unfoldType bodyType)
      argTypes = init typeComponents
      resultType = last typeComponents
  freshParams <-
    mapM
      ( \argTy -> do
          fresh <- freshName "r"
          pure (Label argTy fresh)
      )
      argTypes
  case NonEmpty.nonEmpty freshParams of
    Nothing ->
      -- Not actually a function type (no arg types); nothing to expand.
      pure ([], body)
    Just freshNE -> do
      let freshVars = fmap EVar freshNE
          appType = resultType
          body' = EApp appType body freshVars
      -- If the result is still a function, recurse.
      if isFunction body'
        then do
          (moreParams, body'') <- etaExpand body'
          pure (freshParams ++ moreParams, body'')
        else pure (freshParams, body')
