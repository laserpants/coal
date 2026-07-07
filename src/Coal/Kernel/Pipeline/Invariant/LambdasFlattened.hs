{- |
Invariant checker for lambda flattening (pass 3).

Verifies that no lambda expression directly returns another lambda expression,
i.e., that all lambdas have been flattened into multi-parameter form.

= Checked invariant

No lambda body is itself a lambda: the pattern @fn(…) => fn(…) => …@ must not
appear anywhere.

= Error reporting

Returns one 'NestedLambdaBody' entry for each @fn(…) => fn(…) => …@ pattern
found, together with errors from nested subexpressions.
-}
module Coal.Kernel.Pipeline.Invariant.LambdasFlattened (
  checkLambdasFlattened,
) where

import qualified Data.List.NonEmpty as NonEmpty

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Pipeline.Invariant.Error (InvariantError (..))

{- | Verify that no lambda expression directly returns another lambda
expression, i.e. that all lambdas have been flattened into multi-parameter
form.

Returns an empty list when the invariant holds everywhere, or one
'NestedLambdaBody' entry for each @fn(…) => fn(…) => …@ pattern found,
together with errors from nested sub-expressions.
-}
checkLambdasFlattened :: Expr Type -> [InvariantError]
checkLambdasFlattened expr = case expr of
  EVar _ -> []
  ECon _ -> []
  ELit _ -> []
  ENil -> []
  ELet bindings body ->
    foldMap checkBinding (NonEmpty.toList bindings)
      ++ checkLambdasFlattened body
  ELam _ body ->
    ( case body of
        ELam _ _ -> [NestedLambdaBody]
        _ -> []
    )
      ++ checkLambdasFlattened body
  EApp _ f args ->
    checkLambdasFlattened f
      ++ foldMap checkLambdasFlattened args
  EIf cond t f ->
    checkLambdasFlattened cond
      ++ checkLambdasFlattened t
      ++ checkLambdasFlattened f
  EOp op ->
    foldMap checkLambdasFlattened op
  ECase _ scrutinee clauses ->
    checkLambdasFlattened scrutinee
      ++ foldMap checkClauseBody (NonEmpty.toList clauses)
  EExt _ e1 e2 ->
    checkLambdasFlattened e1
      ++ checkLambdasFlattened e2
  EGet _ e ->
    checkLambdasFlattened e
  ECall _ args k ->
    foldMap checkLambdasFlattened args
      ++ checkLambdasFlattened k

-- | Recurse into a let-binding's definition expression.
checkBinding :: Binding Type -> [InvariantError]
checkBinding (Binding _ e) = checkLambdasFlattened e

-- | Recurse into a clause's body expression.
checkClauseBody :: Clause Type -> [InvariantError]
checkClauseBody (Clause _ body) = checkLambdasFlattened body
