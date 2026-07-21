{- |
Invariant checker for logical operator translation (pass 8).

Verifies that no logical AND (@&&@) or OR (@||@) operator nodes remain anywhere
in the expression tree.

= Checked invariant

After pass 8, every short-circuit boolean composition must have been translated
into an explicit @if-then-else@ expression:

  * @a && b@ becomes @if a then b else false@
  * @a || b@ becomes @if a then true else b@

= Error reporting

Returns one 'AndOperatorPresent' or 'OrOperatorPresent' entry for each
surviving logical operator node, together with errors from nested
subexpressions.

Note: the unary NOT operator (@!@) is not affected by pass 8 and is not checked
here.
-}
module Coal.Kernel.Pipeline.Invariant.LogicalOperatorsTranslated (
  checkLogicalOperatorsTranslated,
) where

import qualified Data.List.NonEmpty as NonEmpty

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..))
import Coal.Kernel.Language.Op (Op (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Pipeline.Invariant.Error (InvariantError (..))

{- | Verify that no logical AND (@&&@) or OR (@||@) operator nodes remain
anywhere in the expression tree.

After pass 8, every short-circuit boolean composition must have been translated
into an explicit @if-then-else@ expression:

  * @a && b@ becomes @if a then b else false@
  * @a || b@ becomes @if a then true else b@

Returns an empty list when the invariant holds everywhere, or one
'AndOperatorPresent' or 'OrOperatorPresent' entry for each surviving logical
operator node, together with errors from nested subexpressions.

Note: the unary NOT operator (@!@) is not affected by pass 8 and is not checked
here.
-}
checkLogicalOperatorsTranslated :: Expr Type -> [InvariantError]
checkLogicalOperatorsTranslated expr = case expr of
  EVar _ -> []
  ECon _ -> []
  ELit _ -> []
  ENil -> []
  ELet bindings body ->
    foldMap checkBinding (NonEmpty.toList bindings)
      ++ checkLogicalOperatorsTranslated body
  ELam _ body ->
    checkLogicalOperatorsTranslated body
  EApp _ f args ->
    checkLogicalOperatorsTranslated f
      ++ foldMap checkLogicalOperatorsTranslated args
  EIf cond t f ->
    checkLogicalOperatorsTranslated cond
      ++ checkLogicalOperatorsTranslated t
      ++ checkLogicalOperatorsTranslated f
  EOp op ->
    ( case op of
        OAnd _ _ -> [AndOperatorPresent]
        OOr _ _ -> [OrOperatorPresent]
        _ -> []
    )
      ++ foldMap checkLogicalOperatorsTranslated op
  ECase _ scrutinee clauses ->
    checkLogicalOperatorsTranslated scrutinee
      ++ foldMap checkClauseBody (NonEmpty.toList clauses)
  EExt _ e1 e2 ->
    checkLogicalOperatorsTranslated e1
      ++ checkLogicalOperatorsTranslated e2
  EGet _ e ->
    checkLogicalOperatorsTranslated e
  ECall _ args k ->
    foldMap checkLogicalOperatorsTranslated args
      ++ checkLogicalOperatorsTranslated k

-- | Recurse into a let-binding's definition expression.
checkBinding :: Binding Type -> [InvariantError]
checkBinding (Binding _ e) = checkLogicalOperatorsTranslated e

-- | Recurse into a clause's body expression.
checkClauseBody :: Clause Type -> [InvariantError]
checkClauseBody (Clause _ body) = checkLogicalOperatorsTranslated body
