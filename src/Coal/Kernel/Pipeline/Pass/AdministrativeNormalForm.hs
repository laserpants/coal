{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Normalization pass 10: Administrative normal form (ANF).

Transforms every expression in the module into administrative normal form,
where all operands are atomic. This simplifies code generation by eliminating
nested complex expressions.

= Invariants established

After this pass:

  1. Every operand (function, argument, operator input, record field/row,
     scrutinee, condition) is __atomic__ (@EVar@, @ECon@, @ELit@, @ENil@).
  2. @EIf@ and @ECase@ in operand (value) position are bound to fresh
     @let@-variables at their point of use, so the surrounding continuation is
     applied exactly once. They may also appear in __tail position__.
  3. Non-atomic subexpressions are extracted into fresh @let@-bindings.

= Example transformation

@
let x = if c then a else b
in body
@

becomes (the control flow is bound to the let-variable, it is /not/ floated
outward into the branches):

@
let x = if c then a else b
in body
@
-}
module Coal.Kernel.Pipeline.Pass.AdministrativeNormalForm (
  administrativeNormalForm,
) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Language.Type.HasType (typeOf)
import Coal.Kernel.Pipeline (Pass, PipelineT, freshName)
import Coal.Kernel.Pipeline.Pass.Utils (opOperands, rebuildOp)

{- | Transform every expression in the module into administrative normal form
(ANF).

= Invariants

After this pass:

  1. Every operand (function, argument, operator input, record field/row,
     scrutinee, condition) is __atomic__ (@EVar@, @ECon@, @ELit@, @ENil@).
  2. @EIf@ and @ECase@ in operand (value) position are bound to fresh
     @let@-variables at their point of use, so the surrounding continuation is
     applied exactly once. They may also appear in __tail position__.
  3. Non-atomic subexpressions are extracted into fresh @let@-bindings.

= Example

@
let x = if c then a else b
in body
@

stays as:

@
let x = if c then a else b
in body
@

(control flow is not floated outward into the branches; this keeps the
transformation linear in the size of the input program).
-}
administrativeNormalForm :: (Monad m) => Pass m (Module Type) (Module Type)
administrativeNormalForm m = do
  objs' <- mapM anfObject (moduleObjects m)
  pure m{moduleObjects = objs'}

-- ---------------------------------------------------------------------------
-- Object
-- ---------------------------------------------------------------------------

anfObject :: (Monad m) => Object Type -> PipelineT m (Object Type)
anfObject =
  \case
    DFunction scope name params body ->
      DFunction scope name params <$> anfTail body
    DConstant name expr ->
      DConstant name <$> anfTail expr
    other ->
      pure other

-- ---------------------------------------------------------------------------
-- Atomicity
-- ---------------------------------------------------------------------------

isAtomic :: Expr Type -> Bool
isAtomic =
  \case
    EVar _ -> True
    ECon _ -> True
    ELit _ -> True
    ENil -> True
    _ -> False

-- ---------------------------------------------------------------------------
-- Tail-position normalization
-- ---------------------------------------------------------------------------

{- | Normalize @expr@ in tail position.

In tail position @EIf@ and @ECase@ are permitted. Their operands are
atomized and their branches are normalized tail-recursively.
-}
anfTail :: (Monad m) => Expr Type -> PipelineT m (Expr Type)
anfTail expr =
  case expr of
    EVar _ -> pure expr
    ECon _ -> pure expr
    ELit _ -> pure expr
    ENil -> pure expr
    ELam params body ->
      ELam params <$> anfTail body
    ELet bs body ->
      anfLetSeq (NonEmpty.toList bs) (anfTail body)
    EIf cond th el ->
      anfValue cond $ \condAtom -> do
        th' <- anfTail th
        el' <- anfTail el
        pure (EIf condAtom th' el')
    ECase t scrutinee clauses ->
      anfValue scrutinee $ \scrutAtom -> do
        clauses' <- mapM anfClause clauses
        pure (ECase t scrutAtom clauses')
    EApp t f args ->
      anfValue f $ \fAtom ->
        anfAll (NonEmpty.toList args) $ \argAtoms ->
          pure (EApp t fAtom (NonEmpty.fromList argAtoms))
    EOp op ->
      anfAll (opOperands op) $ \atoms ->
        pure (EOp (rebuildOp atoms op))
    EExt name e1 e2 ->
      anfValue e1 $ \e1Atom ->
        anfValue e2 $ \e2Atom ->
          pure (EExt name e1Atom e2Atom)
    EGet lbl e ->
      anfValue e $ \eAtom ->
        pure (EGet lbl eAtom)
    ECall (Label t name) args k ->
      anfAll args $ \argAtoms ->
        anfValue k $ \kAtom ->
          pure (ECall (Label t name) argAtoms kAtom)

-- --------------------------------------------------------------------------
-- Operand-position (let-binding RHS) normalization
-- ---------------------------------------------------------------------------
-- Value-position normalization (continuation-passing)
-- ---------------------------------------------------------------------------

{- | Normalize @expr@ in value (operand) position and pass the /atomic/
result to @cont@.

When @expr@ is @EIf@ or @ECase@, the control flow is bound to a fresh
@let@-variable and the continuation is applied exactly once. This avoids
floating a shared continuation into every branch, which would duplicate the
surrounding code for each control-flow expression (quadratic blow-up):

@
  anfValue (if c then a else b) cont
    ≡  let anf.N = if c_atom then a' else b' in cont anf.N
@

Branches and clause bodies are normalized as value expressions (their
operands atomized) with an identity continuation, so they are computed
exactly once.

For all other non-atomic expressions sub-expressions are atomized
(possibly binding control flow from them) and the result is bound
to a fresh name via 'bindFresh'.
-}
anfValue ::
  (Monad m) =>
  Expr Type ->
  (Expr Type -> PipelineT m (Expr Type)) ->
  PipelineT m (Expr Type)
anfValue expr cont
  | isAtomic expr = cont expr
  | otherwise =
      case expr of
        EIf cond th el ->
          anfValue cond $ \condAtom -> do
            th' <- anfLetRhs th pure
            el' <- anfLetRhs el pure
            bindFresh (EIf condAtom th' el') cont
        ECase _t scrutinee clauses ->
          anfValue scrutinee $ \scrutAtom -> do
            clauses' <- mapM (\(Clause ps b) -> Clause ps <$> anfLetRhs b pure) clauses
            -- Derive the case result type from the normalized first branch.
            let newT = case clauses' of Clause _ body :| _ -> typeOf body
            bindFresh (ECase newT scrutAtom clauses') cont
        ELet bs body ->
          anfLetSeq (NonEmpty.toList bs) (anfValue body cont)
        ELam params body -> do
          body' <- anfTail body
          bindFresh (ELam params body') cont
        ECall (Label t name) args k ->
          anfAll args $ \argAtoms ->
            anfValue k $ \kAtom ->
              cont (ECall (Label t name) argAtoms kAtom)
        _ ->
          anfLetRhs expr $
            \expr' -> bindFresh expr' cont

-- ---------------------------------------------------------------------------
-- Let-binding RHS normalization
-- ---------------------------------------------------------------------------

{- | Normalize @expr@ for use as the RHS of a @let@-binding.

The continuation @cont@ receives a fully normalized value expression. If
@expr@ is @EIf@ or @ECase@ the control flow is kept as the binding RHS (the
surrounding computation is applied exactly once, not floated into every
branch); branches and clause bodies are normalized as value expressions with
an identity continuation.
-}
anfLetRhs ::
  (Monad m) =>
  Expr Type ->
  (Expr Type -> PipelineT m (Expr Type)) ->
  PipelineT m (Expr Type)
anfLetRhs expr cont
  | isAtomic expr = cont expr
  | otherwise = case expr of
      EIf cond th el ->
        anfValue cond $ \condAtom -> do
          th' <- anfLetRhs th pure
          el' <- anfLetRhs el pure
          cont (EIf condAtom th' el')
      ECase _t scrutinee clauses ->
        anfValue scrutinee $ \scrutAtom -> do
          clauses' <- mapM (\(Clause ps b) -> Clause ps <$> anfLetRhs b pure) clauses
          let newT = case clauses' of Clause _ body :| _ -> typeOf body
          cont (ECase newT scrutAtom clauses')
      ELet bs body ->
        anfLetSeq (NonEmpty.toList bs) (anfLetRhs body cont)
      ELam params body -> do
        body' <- anfTail body
        cont (ELam params body')
      EApp t f args ->
        anfValue f $ \fAtom ->
          anfAll (NonEmpty.toList args) $ \argAtoms ->
            cont (EApp t fAtom (NonEmpty.fromList argAtoms))
      EOp op ->
        anfAll (opOperands op) $ \atoms ->
          cont (EOp (rebuildOp atoms op))
      EExt name e1 e2 ->
        anfValue e1 $ \e1Atom ->
          anfValue e2 $ \e2Atom ->
            cont (EExt name e1Atom e2Atom)
      EGet lbl e ->
        anfValue e $ \eAtom ->
          cont (EGet lbl eAtom)
      ECall (Label t name) args k ->
        anfAll args $ \argAtoms ->
          anfValue k $ \kAtom ->
            cont (ECall (Label t name) argAtoms kAtom)
      other ->
        cont other

-- ---------------------------------------------------------------------------
-- Let-binding sequence processing
-- ---------------------------------------------------------------------------

{- | Process a sequence of @let@-bindings followed by a body.

Each binding RHS is normalized via 'anfLetRhs'. Control flow in a RHS is kept
as the binding RHS (wrapped in a @let@) rather than floated outward.
-}
anfLetSeq ::
  (Monad m) =>
  [Binding Type] ->
  PipelineT m (Expr Type) ->
  PipelineT m (Expr Type)
anfLetSeq [] bodyM = bodyM
anfLetSeq (Binding lbl rhs : rest) bodyM =
  anfLetRhs rhs $ \rhs' -> do
    inner <- anfLetSeq rest bodyM
    pure (wrapOne (Binding lbl rhs') inner)

-- ---------------------------------------------------------------------------
-- List atomization
-- ---------------------------------------------------------------------------

{- | Atomize each expression in the list via 'anfValue', threading the atoms
through to the continuation. Control flow inside any element is let-bound
rather than floated outward.
-}
anfAll ::
  (Monad m) =>
  [Expr Type] ->
  ([Expr Type] -> PipelineT m (Expr Type)) ->
  PipelineT m (Expr Type)
anfAll [] cont = cont []
anfAll (e : es) cont =
  anfValue e $ \eAtom ->
    anfAll es $ \esAtoms ->
      cont (eAtom : esAtoms)

-- ---------------------------------------------------------------------------
-- Clause
-- ---------------------------------------------------------------------------

anfClause :: (Monad m) => Clause Type -> PipelineT m (Clause Type)
anfClause (Clause params body) = Clause params <$> anfTail body

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

{- | Bind @expr@ to a fresh @let@-variable and pass the variable to @cont@.
If @expr@ is already atomic no binding is introduced.
-}
bindFresh ::
  (Monad m) =>
  Expr Type ->
  (Expr Type -> PipelineT m (Expr Type)) ->
  PipelineT m (Expr Type)
bindFresh expr cont
  | isAtomic expr = cont expr
  | otherwise = do
      fresh <- freshName "anf"
      let lbl = Label (typeOf expr) fresh
      body <- cont (EVar lbl)
      pure (wrapOne (Binding lbl expr) body)

-- | Wrap an expression in a single-binding @let@.
wrapOne :: Binding Type -> Expr Type -> Expr Type
wrapOne b = ELet (NonEmpty.singleton b)
