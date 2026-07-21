{- |
Normalization pass 9: Let-binding simplification and alias elimination.

Removes trivial variable-alias bindings from @let@ expressions and relabels
all variable references through the resulting substitution map. This simplifies
the AST and improves readability of intermediate forms.

= Invariant established

After this pass, no @let@ expression contains a binding of the form
@let x = y in …@ where @y@ is a variable.

= Transformation rules

Applied to each @let@:

  * __Alias binding__: @let x = y in …@ — record @x ↦ resolve(y)@ and
    discard the binding.
  * __Real binding__: any other RHS — keep, but canonicalize the RHS
    expression.
  * __Degenerate let__: if all bindings were aliases, replace the whole @let@
    with the (relabeled) body.

Indirection chains are chased to convergence: if @x ↦ y@ and @y ↦ z@, all
occurrences of @x@ become @z@.
-}
module Coal.Kernel.Pipeline.Pass.LetBindingSimplification (
  letBindingSimplification,
) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Coal.Common.Name (Name)
import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Pipeline (Pass)

{- | Remove trivial variable-alias bindings from @let@ expressions and relabel
all variable references through the resulting substitution map.

= Transformation rules

Applied to each @let@:

  * __Alias binding__: @let x = y in …@ — record @x ↦ resolve(y)@ and
    discard the binding.
  * __Real binding__: any other RHS — keep, but canonicalize the RHS
    expression.
  * __Degenerate let__: if all bindings were aliases, replace the whole @let@
    with the (relabeled) body.

Indirection chains are chased to convergence: if @x ↦ y@ and @y ↦ z@, all
occurrences of @x@ become @z@.
-}
letBindingSimplification :: (Monad m) => Pass m (Module Type) (Module Type)
letBindingSimplification m =
  pure m{moduleObjects = map simplifyObject (moduleObjects m)}

-- --------------------------------------------------------------------------
-- Object
-- --------------------------------------------------------------------------

simplifyObject :: Object Type -> Object Type
simplifyObject obj =
  case obj of
    DFunction scope name params body ->
      DFunction scope name params (simplifyExpr Map.empty body)
    DConstant name expr ->
      DConstant name (simplifyExpr Map.empty expr)
    DExternal{} ->
      obj
    DData{} ->
      obj

-- --------------------------------------------------------------------------
-- Expr
-- --------------------------------------------------------------------------

-- | Simplify an expression, applying the current substitution map @subst@.
simplifyExpr :: Map Name Name -> Expr Type -> Expr Type
simplifyExpr subst expr =
  case expr of
    EVar (Label t name) ->
      EVar (Label t (resolve subst name))
    ECon _ ->
      expr
    ELit _ ->
      expr
    ENil ->
      expr
    ELet bindings body ->
      simplifyLet subst bindings body
    ELam params body ->
      ELam params (simplifyExpr subst body)
    EApp t f args ->
      EApp t (simplifyExpr subst f) (fmap (simplifyExpr subst) args)
    EIf cond th el ->
      EIf (simplifyExpr subst cond) (simplifyExpr subst th) (simplifyExpr subst el)
    EOp op ->
      EOp (fmap (simplifyExpr subst) op)
    ECase t scrutinee clauses ->
      ECase t (simplifyExpr subst scrutinee) (fmap (simplifyClause subst) clauses)
    EExt name e1 e2 ->
      EExt name (simplifyExpr subst e1) (simplifyExpr subst e2)
    EGet lbl e ->
      EGet lbl (simplifyExpr subst e)
    ECall (Label t name) args k ->
      ECall (Label t name) (fmap (simplifyExpr subst) args) (simplifyExpr subst k)

-- --------------------------------------------------------------------------
-- Let simplification
-- --------------------------------------------------------------------------

simplifyLet ::
  Map Name Name ->
  NonEmpty (Binding Type) ->
  Expr Type ->
  Expr Type
simplifyLet subst bindings body =
  let
    -- Partition into alias and real bindings, building an extended subst.
    (subst', realBindings) = foldl processBinding (subst, []) (NonEmpty.toList bindings)
    -- Simplify each real binding's RHS under the extended subst.
    realBindings' = map (\(Binding lbl e) -> Binding lbl (simplifyExpr subst' e)) realBindings
    body' = simplifyExpr subst' body
   in
    case NonEmpty.nonEmpty realBindings' of
      Nothing ->
        -- All bindings were aliases: eliminate the let entirely.
        body'
      Just real ->
        ELet real body'

{- | Process one binding: if it's an alias (@x = y@), extend the substitution;
otherwise keep it as a real binding.
-}
processBinding ::
  (Map Name Name, [Binding Type]) ->
  Binding Type ->
  (Map Name Name, [Binding Type])
processBinding (subst, real) (Binding (Label t name) rhs) =
  case rhs of
    EVar (Label _ rhsName) ->
      -- Alias: record the substitution (chase to final target).
      let target = resolve subst rhsName
       in (Map.insert name target subst, real)
    _ ->
      -- Real binding: keep it.
      (subst, real ++ [Binding (Label t name) rhs])

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

-- | Chase a name through the substitution map to find its final target.
resolve :: Map Name Name -> Name -> Name
resolve subst name =
  case Map.lookup name subst of
    Nothing -> name
    Just target
      | target == name -> name -- break cycles (shouldn't arise after pass 002)
      | otherwise -> resolve subst target

simplifyClause :: Map Name Name -> Clause Type -> Clause Type
simplifyClause subst (Clause params body) =
  Clause params (simplifyExpr subst body)
