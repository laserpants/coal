{- |
Invariant checker for top-level function normalization (pass 6).

Verifies that no top-level object is a constant containing a lambda expression,
and no top-level function has a lambda expression as its direct body.

= Checked invariant

Both forms should have been eliminated by pass 6:

  * @constant N = fn(…) => e@ must be promoted to @function N(…) = e@.
  * @function N(vs) = fn(ws) => e@ must be flattened to @function N(vs, ws) =
    e@.

This check is intentionally shallow: only the direct body/expression of each
'Object' is inspected. A lambda appearing deeper inside the body is not a
violation of this invariant.

= Error reporting

Returns one error per violation: 'ConstantContainsLambda' or
'FunctionBodyIsLambda'.
-}
module Coal.Kernel.Pipeline.Invariant.TopLevelFunctionsNormalized (
  checkTopLevelFunctionsNormalized,
) where

import Coal.Kernel.Language.Expr (Expr (..))
import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Pipeline.Invariant.Error (InvariantError (..))

{- | Verify that no top-level object is a constant containing a lambda
expression, and no top-level function has a lambda expression as its direct
body.

Both forms should have been eliminated by pass 6:

  * @constant N = fn(…) => e@ must be promoted to @function N(…) = e@.
  * @function N(vs) = fn(ws) => e@ must be flattened to @function N(vs, ws) =
    e@.

This check is intentionally shallow: only the direct body/expression of each
'Object' is inspected. A lambda appearing deeper inside the body is not a
violation of this invariant.

Returns an empty list when the invariant holds, or one error per violation.
-}
checkTopLevelFunctionsNormalized :: Object Type -> [InvariantError]
checkTopLevelFunctionsNormalized obj = case obj of
  DConstant name expr ->
    case expr of
      ELam _ _ ->
        [ConstantContainsLambda name]
      _ ->
        []
  DFunction _ name _ body ->
    case body of
      ELam _ _ ->
        [FunctionBodyIsLambda name]
      _ ->
        []
  DExternal{} ->
    []
  DData{} ->
    []
