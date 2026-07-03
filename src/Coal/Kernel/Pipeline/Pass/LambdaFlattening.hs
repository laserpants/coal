{- |
Normalization pass 3: Lambda flattening.

Merges nested lambda abstractions into single lambdas with multiple parameters.
This simplifies lambda lifting and code generation by eliminating the need to
handle curried function definitions.

= Invariant established

After this pass, no lambda expression contains another lambda as its immediate
body. All multi-parameter functions are represented by a single @fn@ with
multiple parameters.

= Transformation

Any chain @fn(v₁) => fn(v₂) => … fn(vₙ) => e@ (n ≥ 2) is collapsed to
@fn(v₁, v₂, …, vₙ) => e@.

Applied recursively until no nested-lambda chains remain.
-}
module Coal.Kernel.Pipeline.Pass.LambdaFlattening (
  lambdaFlattening,
) where

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..))
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Pipeline (Pass)

{- | Merge nested lambda abstractions into a single lambda with multiple
parameters.

Any chain @fn(v₁) => fn(v₂) => … fn(vₙ) => e@ (n ≥ 2) is collapsed to
@fn(v₁, v₂, …, vₙ) => e@. Applied recursively until no nested-lambda chains
remain.
-}
lambdaFlattening :: (Monad m) => Pass m (Module Type) (Module Type)
lambdaFlattening m =
  pure m{moduleObjects = map flattenObject (moduleObjects m)}

-- --------------------------------------------------------------------------
-- Object
-- --------------------------------------------------------------------------

flattenObject :: Object Type -> Object Type
flattenObject obj =
  case obj of
    DFunction scope name params body ->
      DFunction scope name params (flattenExpr body)
    DConstant name expr ->
      DConstant name (flattenExpr expr)
    DExternal{} ->
      obj
    DData{} ->
      obj

-- --------------------------------------------------------------------------
-- Expr
-- --------------------------------------------------------------------------

flattenExpr :: Expr Type -> Expr Type
flattenExpr expr =
  case expr of
    EVar _ ->
      expr
    ECon _ ->
      expr
    ELit _ ->
      expr
    ENil ->
      expr
    ELet bindings body ->
      ELet (fmap flattenBinding bindings) (flattenExpr body)
    ELam params body ->
      -- Flatten the body first, then collect any immediately nested lambda.
      case flattenExpr body of
        ELam innerParams innerBody ->
          -- Merge: prepend outer params to inner params.
          ELam (params <> innerParams) innerBody
        body' ->
          ELam params body'
    EApp t f args ->
      EApp t (flattenExpr f) (fmap flattenExpr args)
    EIf cond t f ->
      EIf (flattenExpr cond) (flattenExpr t) (flattenExpr f)
    EOp op ->
      EOp (fmap flattenExpr op)
    ECase t scrutinee clauses ->
      ECase t (flattenExpr scrutinee) (fmap flattenClause clauses)
    EExt name e1 e2 ->
      EExt name (flattenExpr e1) (flattenExpr e2)
    EGet lbl e ->
      EGet lbl (flattenExpr e)

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

flattenBinding :: Binding Type -> Binding Type
flattenBinding (Binding lbl e) = Binding lbl (flattenExpr e)

flattenClause :: Clause Type -> Clause Type
flattenClause (Clause params body) = Clause params (flattenExpr body)
