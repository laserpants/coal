{- |
Invariant checker for case expression canonicalization (pass 1).

Verifies that every @case@ expression has its clauses sorted in ascending
lexicographic (alphabetical) order by constructor name.

= Checked invariant

For every @case@ expression, each consecutive pair of clauses @(C₁, C₂)@ must
satisfy @name(C₁) < name(C₂)@ lexicographically.

= Error reporting

Returns one 'CaseClausesOutOfOrder' error for each consecutive out-of-order
pair found, together with errors from nested subexpressions.
-}
module Coal.Kernel.Pipeline.Invariant.CaseExpressionsCanonical (
  checkCaseExpressionsCanonical,
) where

import qualified Data.List.NonEmpty as NonEmpty

import Coal.Common.Name (Name)
import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Pipeline.Invariant.Error (InvariantError (..))

{- | Verify that every @case@ expression in the tree has its clauses sorted
in ascending lexicographic order by constructor name.

Returns an empty list when the invariant holds everywhere, or one
'CaseClausesOutOfOrder' entry for each consecutive out-of-order pair found,
together with errors from nested sub-expressions.
-}
checkCaseExpressionsCanonical :: Expr Type -> [InvariantError]
checkCaseExpressionsCanonical expr = case expr of
  EVar _ ->
    []
  ECon _ ->
    []
  ELit _ ->
    []
  ENil ->
    []
  ELet bindings body ->
    foldMap checkBinding (NonEmpty.toList bindings)
      ++ checkCaseExpressionsCanonical body
  ELam _ body ->
    checkCaseExpressionsCanonical body
  EApp _ f args ->
    checkCaseExpressionsCanonical f
      ++ foldMap checkCaseExpressionsCanonical args
  EIf cond t f ->
    checkCaseExpressionsCanonical cond
      ++ checkCaseExpressionsCanonical t
      ++ checkCaseExpressionsCanonical f
  EOp op ->
    foldMap checkCaseExpressionsCanonical op
  ECase _ scrutinee clauses ->
    let names = map clauseConName (NonEmpty.toList clauses)
        orderErrors = concat (zipWith checkPair names (drop 1 names))
        scrutineeErrors = checkCaseExpressionsCanonical scrutinee
        clauseErrors = foldMap checkClauseBody (NonEmpty.toList clauses)
     in orderErrors ++ scrutineeErrors ++ clauseErrors
  EExt _ e1 e2 ->
    checkCaseExpressionsCanonical e1
      ++ checkCaseExpressionsCanonical e2
  EGet _ e ->
    checkCaseExpressionsCanonical e
  ECall _ args k ->
    foldMap checkCaseExpressionsCanonical args
      ++ checkCaseExpressionsCanonical k
 where
  checkBinding (Binding _ e) = checkCaseExpressionsCanonical e
  checkClauseBody (Clause _ body) = checkCaseExpressionsCanonical body

-- | Extract the constructor name from a clause (the name of the first label).
clauseConName :: Clause t -> Name
clauseConName (Clause labels _) =
  let Label _ name = NonEmpty.head labels
   in name

{- | Emit 'CaseClausesOutOfOrder' when two adjacent constructor names are not
in ascending lexicographic order.
-}
checkPair :: Name -> Name -> [InvariantError]
checkPair a b
  | a < b = []
  | otherwise = [CaseClausesOutOfOrder a b]
