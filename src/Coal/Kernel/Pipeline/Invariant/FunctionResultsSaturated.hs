{- |
Invariant checker for function results saturation (pass 7).

Verifies that no top-level function has a result type that is itself a function
type.

= Checked invariant

After pass 7 (function results saturation), all functions should return
non-function values. A function with a functional result type should have been
saturated with additional parameters to eliminate the functional result.

For each top-level function, this check verifies that @typeOf(body)@ is not a
function type. In other words, the result type must not be of the form @A →
B@.

= Error reporting

Returns one 'FunctionResultIsFunction' error per violation.
-}
module Coal.Kernel.Pipeline.Invariant.FunctionResultsSaturated (
  checkFunctionResultsSaturated,
) where

import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Language.Type.Function (isFunction)
import Coal.Kernel.Pipeline.Invariant.Error (InvariantError (..))

{- | Verify that no top-level function has a result type that is itself a
function type.

After pass 7 (function results saturation), all functions should return
non-function values. A function with a functional result type should have been
saturated with additional parameters to eliminate the functional result.

For each top-level function, this check verifies that @typeOf(body)@ is not a
function type. In other words, the result type must not be of the form @A →
B@.

Returns an empty list when the invariant holds, or one error per violation.
-}
checkFunctionResultsSaturated :: Object Type -> [InvariantError]
checkFunctionResultsSaturated obj = case obj of
  DFunction _ name _ body ->
    ([FunctionResultIsFunction name | isFunction body])
  DConstant name body ->
    [FunctionResultIsFunction name | isFunction body]
  DExternal{} ->
    []
  DData{} ->
    []
