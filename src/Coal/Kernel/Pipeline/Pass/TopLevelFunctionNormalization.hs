{- |
Normalization pass 6: Top-level function normalization.

Normalizes all top-level definitions so that no callable object is represented
as a lambda-valued constant or as a function whose body is a lambda
abstraction. This establishes a uniform calling convention where all functions
are defined with their full parameter lists.

= Invariant established

After this pass:

  * No 'DFunction' has a lambda expression as its immediate body
  * No 'DConstant' has a lambda expression as its RHS

= Transformation rules

__Rule 1__ — Flatten function bodies:
@function foo(v₁, …, vₙ) = fn(w₁, …, wₘ) => e@ becomes
@function foo(v₁, …, vₙ, w₁, …, wₘ) = e@

__Rule 2__ — Promote constant lambdas:
@constant foo = fn(v₁, …, vₙ) => e@ becomes @function foo(v₁, …, vₙ) = e@

= Preconditions

Lambda flattening has already run, which implies that no lambda body is itself
a lambda.
-}
module Coal.Kernel.Pipeline.Pass.TopLevelFunctionNormalization (
  topLevelFunctionNormalization,
) where

import qualified Data.List.NonEmpty as NonEmpty

import Coal.Kernel.Language.Expr (Expr (..))
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (FunctionScope (..), Object (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Pipeline (Pass)

{- | Normalize all top-level definitions so that no callable object is
represented as a lambda-valued constant or as a function whose body is a lambda
abstraction.

__Rule 1__ — Flatten function bodies:
@function foo(v₁, …, vₙ) = fn(w₁, …, wₘ) => e@ becomes
@function foo(v₁, …, vₙ, w₁, …, wₘ) = e@

__Rule 2__ — Promote constant lambdas:
@constant foo = fn(v₁, …, vₙ) => e@ becomes @function foo(v₁, …, vₙ) = e@

__Precondition__: lambda flattening has already run, which implies that no
lambda body is itself a lambda.
-}
topLevelFunctionNormalization :: (Monad m) => Pass m (Module Type) (Module Type)
topLevelFunctionNormalization m =
  pure m{moduleObjects = map normalizeObject (moduleObjects m)}

-- --------------------------------------------------------------------------
-- Object
-- --------------------------------------------------------------------------

normalizeObject :: Object Type -> Object Type
normalizeObject obj =
  case obj of
    -- Rule 1: function whose body is a lambda → merge params.
    DFunction scope name params (ELam innerParams body) ->
      DFunction scope name (params ++ NonEmpty.toList innerParams) body
    -- Rule 2: constant whose RHS is a lambda → promote to function.
    DConstant name (ELam params body) ->
      DFunction Exported name (NonEmpty.toList params) body
    -- Everything else is already normalized.
    DFunction{} ->
      obj
    DConstant{} ->
      obj
    DExternal{} ->
      obj
    DData{} ->
      obj
