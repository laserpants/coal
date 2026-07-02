{-# LANGUAGE LambdaCase #-}

{- |
Normalization pass 1: Case expression canonicalization.

Sorts the clauses of every @case@ expression into ascending lexicographic
order by constructor name. The LLVM code generator assigns switch-case values
by clause position, so clauses must appear in a consistent canonical order for
dispatch to be correct.

= Invariant established

After this pass, all @case@ expressions have their clauses sorted in ascending
lexicographic order by constructor name.

= Implementation

This is a pure restructuring pass: no expressions are added or removed, and no
names are changed. The only observable effect is the reordering of case
clauses.

With the grouped DData representation, constructor indices are implicitly
assigned by lexicographic position in the constructor list, so explicit
validation is no longer needed.
-}
module Coal.Kernel.Pipeline.Pass.CaseExpressionCanonicalization (
  caseExpressionCanonicalization,
) where

import Data.List (sortBy)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Ord (comparing)

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Pipeline (Pass)
import Common (Name)

{- | Sort the clauses of every @case@ expression into ascending lexicographic
order by constructor name.

Clauses are sorted alphabetically by constructor name, which is consistent
across all modules regardless of where the constructors are defined.

With the grouped DData representation, constructor indices are implicitly
assigned by lexicographic position, so index validation is no longer needed.
-}
caseExpressionCanonicalization :: (Monad m) => Pass m (Module Type) (Module Type)
caseExpressionCanonicalization m = do
  pure m{moduleObjects = map canonicalizeObject (moduleObjects m)}

-- --------------------------------------------------------------------------
-- Object
-- --------------------------------------------------------------------------

canonicalizeObject :: Object Type -> Object Type
canonicalizeObject =
  \case
    DFunction name params body ->
      DFunction name params (canonicalizeExpr body)
    DConstant name expr ->
      DConstant name (canonicalizeExpr expr)
    obj@DExternal{} ->
      obj
    obj@DData{} ->
      obj

-- --------------------------------------------------------------------------
-- Expr
-- --------------------------------------------------------------------------

canonicalizeExpr :: Expr Type -> Expr Type
canonicalizeExpr expr =
  case expr of
    EVar{} ->
      expr
    ECon{} ->
      expr
    ELit{} ->
      expr
    ENil ->
      expr
    ELet bindings body ->
      ELet (fmap (canonicalizeBinding) bindings) (canonicalizeExpr body)
    ELam params body ->
      ELam params (canonicalizeExpr body)
    EApp t f args ->
      EApp t (canonicalizeExpr f) (fmap canonicalizeExpr args)
    EIf cond t f ->
      EIf
        (canonicalizeExpr cond)
        (canonicalizeExpr t)
        (canonicalizeExpr f)
    EOp op ->
      EOp (fmap canonicalizeExpr op)
    ECase t scrutinee clauses ->
      ECase t (canonicalizeExpr scrutinee) (fmap canonicalizeClause sorted)
     where
      sorted =
        NonEmpty.fromList $
          sortBy (comparing clauseSortKey) (NonEmpty.toList clauses)
    EExt name e1 e2 ->
      EExt name (canonicalizeExpr e1) (canonicalizeExpr e2)
    EGet lbl e ->
      EGet lbl (canonicalizeExpr e)

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

canonicalizeBinding :: Binding Type -> Binding Type
canonicalizeBinding (Binding lbl e) = Binding lbl (canonicalizeExpr e)

canonicalizeClause :: Clause Type -> Clause Type
canonicalizeClause (Clause params body) = Clause params (canonicalizeExpr body)

{- | The sort key for a clause: the constructor name, giving lexicographic
(alphabetical) ordering across all clauses.
-}
clauseSortKey :: Clause t -> Name
clauseSortKey (Clause labels _) =
  let Label _ name = NonEmpty.head labels
   in name
