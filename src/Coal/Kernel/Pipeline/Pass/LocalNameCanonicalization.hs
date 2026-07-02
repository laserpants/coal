{- |
Normalization pass 2: Local name canonicalization.

Alpha-renames every locally bound name to a globally unique @x.[n]@ identifier.
This eliminates shadowing and simplifies later passes by ensuring every
binding site has a distinct name.

= Invariant established

After this pass, every locally bound variable has a unique name across the
entire program. Top-level function names, data constructor names, and type
names are left untouched.

= Implementation

Affected binders:

  * @let@ bindings
  * Lambda parameters
  * @case@ pattern variables

The pass threads a fresh-name counter through the 'PipelineT' monad to
generate unique identifiers.
-}
module Coal.Kernel.Pipeline.Pass.LocalNameCanonicalization (
  localNameCanonicalization,
) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Pipeline (Pass, PipelineT, freshName)
import Common (Name)

{- | Alpha-rename every locally bound name to a globally unique @x.[n]@
identifier.

Affected binders: @let@ bindings, lambda parameters, @case@ pattern variables.
Top-level function names, data constructor names, and type names are left
untouched.
-}
localNameCanonicalization :: (Monad m) => Pass m (Module Type) (Module Type)
localNameCanonicalization m = do
  newObjects <- mapM canonicalizeObject (moduleObjects m)
  pure m{moduleObjects = newObjects}

-- --------------------------------------------------------------------------
-- Object
-- --------------------------------------------------------------------------

canonicalizeObject :: (Monad m) => Object Type -> PipelineT m (Object Type)
canonicalizeObject obj =
  case obj of
    DFunction name params body -> do
      -- Parameters at top level are NOT locally-bound in the "local name"
      -- sense; they belong to the top-level binding, so we do not rename them
      -- here. However we do rename everything inside the body.
      body' <- canonicalizeExpr Map.empty body
      pure (DFunction name params body')
    DConstant name expr -> do
      expr' <- canonicalizeExpr Map.empty expr
      pure (DConstant name expr')
    DExternal{} ->
      pure obj
    DData{} ->
      pure obj

-- --------------------------------------------------------------------------
-- Expr
-- --------------------------------------------------------------------------

-- | Rename an expression, threading a substitution map (old → new) through.
canonicalizeExpr :: (Monad m) => Map Name Name -> Expr Type -> PipelineT m (Expr Type)
canonicalizeExpr env expr =
  case expr of
    EVar (Label t name) ->
      pure (EVar (Label t (applyEnv env name)))
    ECon _ ->
      pure expr
    ELit _ ->
      pure expr
    ENil ->
      pure expr
    ELet bindings body -> do
      -- Allocate fresh names for every binder in this let group first, so they
      -- are all in scope together (mirroring the mutual scoping of let).
      (env', bindings') <- renameLet env (NonEmpty.toList bindings)
      body' <- canonicalizeExpr env' body
      pure (ELet (NonEmpty.fromList bindings') body')
    ELam params body -> do
      -- Allocate fresh names for lambda parameters.
      (env', params') <- renameLabels env (NonEmpty.toList params)
      body' <- canonicalizeExpr env' body
      pure (ELam (NonEmpty.fromList params') body')
    EApp t f args -> do
      f' <- canonicalizeExpr env f
      args' <- mapM (canonicalizeExpr env) args
      pure (EApp t f' args')
    EIf cond t f -> do
      cond' <- canonicalizeExpr env cond
      t' <- canonicalizeExpr env t
      f' <- canonicalizeExpr env f
      pure (EIf cond' t' f')
    EOp op -> do
      op' <- mapM (canonicalizeExpr env) op
      pure (EOp op')
    ECase t scrutinee clauses -> do
      scrutinee' <- canonicalizeExpr env scrutinee
      clauses' <- mapM (canonicalizeClause env) clauses
      pure (ECase t scrutinee' clauses')
    EExt name e1 e2 -> do
      e1' <- canonicalizeExpr env e1
      e2' <- canonicalizeExpr env e2
      pure (EExt name e1' e2')
    EGet (Label t name) e -> do
      e' <- canonicalizeExpr env e
      -- Field labels in EGet use the label name as a field, not a variable.
      pure (EGet (Label t name) e')

-- --------------------------------------------------------------------------
-- Case clause
-- --------------------------------------------------------------------------

{- | Rename the pattern-bound variables (every label after the first, which is
the constructor) and then recurse into the body.
-}
canonicalizeClause :: (Monad m) => Map Name Name -> Clause Type -> PipelineT m (Clause Type)
canonicalizeClause env (Clause labels body) = do
  let con = NonEmpty.head labels
      patVars = NonEmpty.tail labels
  (env', patVars') <- renameLabels env patVars
  body' <- canonicalizeExpr env' body
  pure (Clause (con NonEmpty.:| patVars') body')

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

applyEnv :: Map Name Name -> Name -> Name
applyEnv env name = Map.findWithDefault name name env

-- | Allocate fresh names for a list of labels, extending the env.
renameLabels :: (Monad m) => Map Name Name -> [Label Type] -> PipelineT m (Map Name Name, [Label Type])
renameLabels env [] = pure (env, [])
renameLabels env (Label t name : rest) = do
  name' <- freshName name
  let env' = Map.insert name name' env
  (env'', rest') <- renameLabels env' rest
  pure (env'', Label t name' : rest')

{- | Allocate fresh names for let-bindings, extending the env. The RHS of each
binding is canonicalized under the *extended* env (so that mutually
referencing bindings work) but also under the original env for the RHS
variable occurrences (we extend all binders first, then canonicalize RHSs).
-}
renameLet :: (Monad m) => Map Name Name -> [Binding Type] -> PipelineT m (Map Name Name, [Binding Type])
renameLet env bindings = do
  -- First pass: allocate all fresh names.
  let oldNames = [name | Binding (Label _ name) _ <- bindings]
  newNames <- mapM freshName oldNames
  let env' = foldr (uncurry Map.insert) env (zip oldNames newNames)
  -- Second pass: canonicalize each RHS under the extended env.
  bindings' <- mapM (canonicalizeBinding env') bindings
  -- Patch the binder labels themselves to use the new names.
  let rename (Binding (Label t _) e, newName) = Binding (Label t newName) e
      bindings'' = zipWith (curry rename) bindings' newNames
  pure (env', bindings'')

canonicalizeBinding :: (Monad m) => Map Name Name -> Binding Type -> PipelineT m (Binding Type)
canonicalizeBinding env (Binding lbl e) = do
  e' <- canonicalizeExpr env e
  pure (Binding lbl e')
