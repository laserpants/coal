{-# LANGUAGE LambdaCase #-}

{- |
Normalization pass 8: Logical operator translation.

Replaces logical operators with explicit @if@-expressions, preserving
short-circuit semantics. This eliminates special-case handling of boolean
operators in later passes.

= Invariant established

After this pass, no @&&@ or @||@ operators remain in the program. All boolean
logic is expressed via @if@ expressions.

= Transformation

  * @a && b@ → @if a then b else false@
  * @a || b@ → @if a then true else b@

Short-circuit semantics are preserved. Applied recursively until no @&&@ or
@||@ operators remain.
-}
module Coal.Kernel.Pipeline.Pass.LogicalOperatorTranslation (
  logicalOperatorTranslation,
) where

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Op (Op (..))
import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Pipeline (Pass)

{- | Replace logical operators with explicit @if@-expressions.

  * @a && b@ → @if a then b else false@
  * @a || b@ → @if a then true else b@

Short-circuit semantics are preserved. Applied recursively until no @&&@ or
@||@ operators remain.
-}
logicalOperatorTranslation :: (Monad m) => Pass m (Module Type) (Module Type)
logicalOperatorTranslation m =
  return m{moduleObjects = map translateObject (moduleObjects m)}

-- --------------------------------------------------------------------------
-- Object
-- --------------------------------------------------------------------------

translateObject :: Object Type -> Object Type
translateObject =
  \case
    DFunction scope name params body ->
      DFunction scope name params (translateExpr body)
    DConstant name expr ->
      DConstant name (translateExpr expr)
    obj@DExternal{} ->
      obj
    obj@DData{} ->
      obj

-- --------------------------------------------------------------------------
-- Expr
-- --------------------------------------------------------------------------

translateExpr :: Expr Type -> Expr Type
translateExpr =
  \case
    expr@EVar{} ->
      expr
    expr@ECon{} ->
      expr
    expr@ELit{} ->
      expr
    expr@ENil ->
      expr
    ELet bindings body ->
      ELet (fmap translateBinding bindings) (translateExpr body)
    ELam params body ->
      ELam params (translateExpr body)
    EApp t f args ->
      EApp t (translateExpr f) (fmap translateExpr args)
    EIf cond th el ->
      EIf (translateExpr cond) (translateExpr th) (translateExpr el)
    EOp op ->
      case op of
        OAnd a b ->
          -- a && b  →  if a then b else false
          EIf (translateExpr a) (translateExpr b) (ELit (PBool False))
        OOr a b ->
          -- a || b  →  if a then true else b
          EIf (translateExpr a) (ELit (PBool True)) (translateExpr b)
        _ ->
          -- All other operators: recurse into operands.
          EOp (fmap translateExpr op)
    ECase t scrutinee clauses ->
      ECase t (translateExpr scrutinee) (fmap translateClause clauses)
    EExt name e1 e2 ->
      EExt name (translateExpr e1) (translateExpr e2)
    EGet lbl e ->
      EGet lbl (translateExpr e)
    ECall (Label t name) args k ->
      ECall (Label t name) (fmap translateExpr args) (translateExpr k)

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

translateBinding :: Binding Type -> Binding Type
translateBinding (Binding lbl e) = Binding lbl (translateExpr e)

translateClause :: Clause Type -> Clause Type
translateClause (Clause params body) = Clause params (translateExpr body)
