{-# LANGUAGE OverloadedStrings #-}

{- |
Normalization pass 5: Lambda lifting.

Lifts all lambda expressions to top-level functions, parameterizing over free
variables. This eliminates nested function definitions, making the program
suitable for compilation to a flat namespace like LLVM IR.

= Invariant established

After this pass, no lambda expressions remain in function bodies. All
functions are defined at the top level of modules.

= Transformation

For each @ELam params body@:

  1. Compute free variables of @body@ (minus the lambda's own params),
     excluding globally defined top-level names.
  2. Sort the free-variable labels by name for determinism.
  3. Generate a fresh top-level name.
  4. Emit @DFunction freshName (fvParams ++ params) body'@.
  5. Replace the @ELam@ site:
     * If there are captured free variables: @EApp (originalLambdaType) (EVar
       liftedName) fvArgs@
     * Otherwise: @EVar (Label lambdaType liftedName)@

Newly lifted functions are appended to 'moduleObjects'.

= Preconditions

Passes 2 (local name canonicalization) and 3 (lambda flattening) have already
run.
-}
module Coal.Kernel.Pipeline.Pass.LambdaLifting (
  lambdaLifting,
) where

import Data.Functor (unzip)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (unzip)

import Coal.Common.Name (Name)
import Coal.Kernel.FreeVars (freeVars)
import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (FunctionScope (..), Object (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Language.Type.HasType (foldType, typeOf)
import Coal.Kernel.Pipeline (Pass, PipelineT, freshName)
import Coal.Kernel.Pipeline.Pass.Utils (opOperands, rebuildOp)

{- | Lift all lambda expressions to top-level functions, parameterizing over
free variables.

For each @ELam params body@:

  1. Compute free variables of @body@ (minus the lambda's own params),
     excluding globally defined top-level names.
  2. Sort the free-variable labels by name for determinism.
  3. Generate a fresh top-level name.
  4. Emit @DFunction freshName (fvParams ++ params) body'@.
  5. Replace the @ELam@ site:
     * If there are captured free variables: @EApp (originalLambdaType) (EVar
       liftedName) fvArgs@
     * Otherwise: @EVar (Label lambdaType liftedName)@

Newly lifted functions are appended to 'moduleObjects'.

__Preconditions__: passes 2 (local name canonicalization) and 3 (lambda
flattening) have already run.
-}

{- | Map from bound variable names to their labels at the binding site.
Threaded through lifting so captured free variables are resolved to the
correct (most-general) binding-site type rather than a use-site specialisation.
-}
type Scope = Map Name (Label Type)

lambdaLifting :: (Monad m) => Pass m (Module Type) (Module Type)
lambdaLifting m = do
  let globals = Set.fromList (concatMap objectNames (moduleObjects m))
  (objs', newFns) <- liftObjects globals (moduleObjects m)
  pure m{moduleObjects = objs' <> newFns}

-- --------------------------------------------------------------------------
-- Object
-- --------------------------------------------------------------------------

objectNames :: Object t -> [Name]
objectNames obj =
  case obj of
    DFunction _ n _ _ -> [n]
    DConstant n _ -> [n]
    DExternal n _ -> [n]
    DData _ ctors -> fst <$> ctors

liftObjects ::
  (Monad m) =>
  Set Name ->
  [Object Type] ->
  PipelineT m ([Object Type], [Object Type])
liftObjects _ [] = pure ([], [])
liftObjects globals (obj : rest) = do
  (obj', new1) <- liftObject globals obj
  (rest', new2) <- liftObjects globals rest
  pure (obj' : rest', new1 <> new2)

liftObject ::
  (Monad m) =>
  Set Name ->
  Object Type ->
  PipelineT m (Object Type, [Object Type])
liftObject globals obj =
  case obj of
    DFunction fscope name params body -> do
      let scope = Map.fromList [(n, lbl) | lbl@(Label _ n) <- params]
      (body', new) <- liftExpr globals scope body
      pure (DFunction fscope name params body', new)
    DConstant name expr -> do
      (expr', new) <- liftExpr globals Map.empty expr
      pure (DConstant name expr', new)
    DExternal{} ->
      pure (obj, [])
    DData{} ->
      pure (obj, [])

-- --------------------------------------------------------------------------
-- Expr
-- --------------------------------------------------------------------------

liftExpr ::
  (Monad m) =>
  Set Name ->
  Scope ->
  Expr Type ->
  PipelineT m (Expr Type, [Object Type])
liftExpr globals scope expr =
  case expr of
    EVar _ ->
      pure (expr, [])
    ECon _ ->
      pure (expr, [])
    ELit _ ->
      pure (expr, [])
    ENil ->
      pure (expr, [])
    ELet bindings body -> do
      (bindings', new1) <- liftBindings globals scope bindings
      let letScope =
            Map.fromList [(n, lbl) | Binding lbl@(Label _ n) _ <- NonEmpty.toList bindings]
              `Map.union` scope
      (body', new2) <- liftExpr globals letScope body
      pure (ELet bindings' body', new1 <> new2)
    ELam params body -> do
      -- 1. Recursively lift any inner lambdas first, extending scope with this
      --    lambda's own params so nested lambdas resolve types correctly.
      let paramNameSet = Set.fromList [n | Label _ n <- NonEmpty.toList params]
          innerScope =
            Map.fromList [(n, lbl) | lbl@(Label _ n) <- NonEmpty.toList params]
              `Map.union` scope
      (body', newFromBody) <- liftExpr globals innerScope body
      -- 2. Compute free variables of the lifted body, excluding lambda params
      --    and globally-defined names.  Deduplicate by name, resolving each
      --    to its binding-site label from the outer scope.
      let fvAll = Set.toList (freeVars body')
          fvNameSet =
            Set.fromList
              [ n
              | Label _ n <- fvAll
              , not (Set.member n paramNameSet)
              , not (Set.member n globals)
              ]
          resolveFv n =
            case Map.lookup n scope of
              Just lbl -> lbl
              Nothing -> case [l | l@(Label _ n') <- fvAll, n' == n] of
                (lbl : _) -> lbl
                [] -> error "resolveFv: free variable not found in fvAll"
          fvLabels = sortBy (comparing labelName) (resolveFv <$> Set.toList fvNameSet)
      -- 3. Mint a fresh top-level name.
      liftedName <- freshName "lam"
      -- 4. Build the parameter list for the lifted function.
      let allParams = fvLabels <> NonEmpty.toList params
      let liftedFnType = foldType (typeOf body') (typeOf <$> allParams)
      let liftedFn = DFunction Local liftedName allParams body'
      -- 5. Build the call site expression.
      let lambdaType = foldType (typeOf body') (typeOf <$> NonEmpty.toList params)
      let callSite = case NonEmpty.nonEmpty fvLabels of
            Nothing ->
              -- No captured free variables: just reference the lifted function.
              EVar (Label lambdaType liftedName)
            Just fvNE ->
              -- Apply the lifted function to the captured free variables.
              EApp lambdaType (EVar (Label liftedFnType liftedName)) (EVar <$> fvNE)
      pure (callSite, newFromBody <> [liftedFn])
    EApp t f args -> do
      (f', new1) <- liftExpr globals scope f
      (argList, new2) <- liftAll globals scope (NonEmpty.toList args)
      pure (EApp t f' (NonEmpty.fromList argList), new1 <> new2)
    EIf cond th el -> do
      (cond', new1) <- liftExpr globals scope cond
      (th', new2) <- liftExpr globals scope th
      (el', new3) <- liftExpr globals scope el
      pure (EIf cond' th' el', new1 <> new2 <> new3)
    EOp op -> do
      let operands = opOperands op
      pairs <- mapM (liftExpr globals scope) operands
      let (lifted, news) = unzip pairs
          op' = rebuildOp lifted op
      pure (EOp op', concat news)
    ECase t scrutinee clauses -> do
      (scrutinee', new1) <- liftExpr globals scope scrutinee
      (clauses', new2) <- liftClauses globals scope clauses
      pure (ECase t scrutinee' clauses', new1 <> new2)
    EExt name e1 e2 -> do
      (e1', new1) <- liftExpr globals scope e1
      (e2', new2) <- liftExpr globals scope e2
      pure (EExt name e1' e2', new1 <> new2)
    EGet lbl e -> do
      (e', new) <- liftExpr globals scope e
      pure (EGet lbl e', new)
    ECall (Label t name) args k -> do
      (args', new1) <- liftAll globals scope args
      (k', new2) <- liftExpr globals scope k
      pure (ECall (Label t name) args' k', new1 <> new2)

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

labelName :: Label t -> Name
labelName (Label _ n) = n

liftAll ::
  (Monad m) =>
  Set Name ->
  Scope ->
  [Expr Type] ->
  PipelineT m ([Expr Type], [Object Type])
liftAll _ _ [] = pure ([], [])
liftAll globals scope (e : es) = do
  (e', new1) <- liftExpr globals scope e
  (es', new2) <- liftAll globals scope es
  pure (e' : es', new1 <> new2)

liftBindings ::
  (Monad m) =>
  Set Name ->
  Scope ->
  NonEmpty (Binding Type) ->
  PipelineT m (NonEmpty (Binding Type), [Object Type])
liftBindings globals scope bindings = do
  pairs <- mapM (liftBinding globals scope) bindings
  let (bs, news) = unzip pairs
  pure (bs, concat (NonEmpty.toList news))

{- | Lift a single let binding.  When the RHS is an 'ELam' whose body
references the binding name (self-recursion), the binding name is excluded
from the captured free-variable list and instead substituted with the
lifted function's call site inside the body, breaking the circular
fixpoint that would otherwise result.
-}
liftBinding ::
  (Monad m) =>
  Set Name ->
  Scope ->
  Binding Type ->
  PipelineT m (Binding Type, [Object Type])
liftBinding globals scope (Binding lbl e) = case e of
  ELam params body -> do
    -- Lift inner lambdas first, extending scope with this lambda's own params.
    let paramNameSet = Set.fromList [n | Label _ n <- NonEmpty.toList params]
        innerScope =
          Map.fromList [(n, l) | l@(Label _ n) <- NonEmpty.toList params]
            `Map.union` scope
    (body', newFromBody) <- liftExpr globals innerScope body
    let bindingName = labelName lbl
        fvAll = Set.toList (freeVars body')
        isRecursive = any (\(Label _ n) -> n == bindingName) fvAll
        -- Collect distinct free-variable names (not own params, not globals,
        -- not self-reference for recursive bindings).
        fvNameSet =
          Set.fromList
            [ n
            | Label _ n <- fvAll
            , not (Set.member n paramNameSet)
            , not (Set.member n globals)
            , not (isRecursive && n == bindingName)
            ]
        -- Resolve each name to its binding-site label from the outer scope.
        resolveFv n =
          case Map.lookup n scope of
            Just l -> l
            Nothing -> case [l | l@(Label _ n') <- fvAll, n' == n] of
              (lbl_ : _) -> lbl_
              [] -> error "resolveFv: free variable not found in fvAll"
        fvLabels = sortBy (comparing labelName) (resolveFv <$> Set.toList fvNameSet)
    liftedName <- freshName "lam"
    let allParams = fvLabels <> NonEmpty.toList params
        liftedFnType = foldType (typeOf body') (typeOf <$> allParams)
        lambdaType = foldType (typeOf body') (typeOf <$> NonEmpty.toList params)
        callSite = case NonEmpty.nonEmpty fvLabels of
          Nothing ->
            EVar (Label lambdaType liftedName)
          Just fvNE ->
            EApp lambdaType (EVar (Label liftedFnType liftedName)) (EVar <$> fvNE)
        -- For recursive lambdas, replace the self-reference with callSite so
        -- the lifted function calls itself by its new top-level name.
        body'' = if isRecursive then substituteVar bindingName callSite body' else body'
        liftedFn = DFunction Local liftedName allParams body''
    pure (Binding lbl callSite, newFromBody <> [liftedFn])
  _ -> do
    (e', new) <- liftExpr globals scope e
    pure (Binding lbl e', new)

liftClauses ::
  (Monad m) =>
  Set Name ->
  Scope ->
  NonEmpty (Clause Type) ->
  PipelineT m (NonEmpty (Clause Type), [Object Type])
liftClauses globals scope clauses = do
  pairs <-
    mapM
      ( \(Clause params body) -> do
          let clauseScope =
                case params of
                  _ :| rest -> Map.fromList [(n, lbl) | lbl@(Label _ n) <- rest]
                  `Map.union` scope
          (body', new) <- liftExpr globals clauseScope body
          pure (Clause params body', new)
      )
      clauses
  let (cs, news) = unzip pairs
  pure (cs, concat (NonEmpty.toList news))

-- --------------------------------------------------------------------------
-- Substitution
-- --------------------------------------------------------------------------

{- | Replace all free occurrences of the variable named @n@ in an expression
with @replacement@.

Substitution stops at any binder that shadows @n@:

* An 'ELam' whose parameter list contains @n@.
* An 'ELet' whose binding list contains @n@ (letrec semantics: @n@ is bound
  for both the RHSes and the body).
* A 'ECase' clause whose pattern-variable list (tail of the label list)
  contains @n@.
-}
substituteVar :: Name -> Expr Type -> Expr Type -> Expr Type
substituteVar n replacement = go
 where
  go expr = case expr of
    EVar (Label _ v)
      | v == n -> replacement
      | otherwise -> expr
    ECon _ -> expr
    ELit _ -> expr
    ENil -> expr
    ELet bindings body
      | any (\(Binding (Label _ bn) _) -> bn == n) (NonEmpty.toList bindings) ->
          -- n is rebound here (letrec); leave the entire let untouched.
          expr
      | otherwise ->
          ELet ((\(Binding l e) -> Binding l (go e)) <$> bindings) (go body)
    ELam params body
      | any (\(Label _ pn) -> pn == n) (NonEmpty.toList params) ->
          -- n is a lambda parameter; do not substitute inside.
          expr
      | otherwise ->
          ELam params (go body)
    EApp t f args -> EApp t (go f) (go <$> args)
    EIf cond th el -> EIf (go cond) (go th) (go el)
    EOp op -> EOp (go <$> op)
    ECase t scrutinee clauses ->
      ECase t (go scrutinee) (goClause <$> clauses)
    EExt name e1 e2 -> EExt name (go e1) (go e2)
    EGet l e -> EGet l (go e)
    ECall label args k -> ECall label (go <$> args) (go k)
  goClause (Clause labels body) =
    let (_ :| sndLabels) = labels
     in if any (\(Label _ pn) -> pn == n) sndLabels
          then -- n is a pattern variable; do not substitute in the clause body.
            Clause labels body
          else Clause labels (go body)
