{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Normalization pass 9: Administrative normal form (ANF).

Transforms every expression in the module into administrative normal form,
where all operands are atomic. This simplifies code generation by eliminating
nested complex expressions.

= Invariants established

After this pass:

  1. Every operand (function, argument, operator input, record field/row,
     scrutinee, condition) is __atomic__ (@EVar@, @ECon@, @ELit@, @ENil@),
     with the single exception of @ECall@ (see 'anfExpr').
  2. @EIf@ and @ECase@ in operand (value) position are bound to fresh
     @let@-variables at their point of use, so the surrounding continuation is
     applied exactly once. They may also appear in __tail position__.
  3. Non-atomic subexpressions are extracted into fresh @let@-bindings.

= Example transformation

@
f(if c then a else b)
@

becomes (the @if@ sits in operand position, so the control flow is bound to a
fresh @let@-variable; the surrounding computation is applied exactly once and
is /not/ floated outward into the branches):

@
let anf.N = if c then a else b
in f(anf.N)
@

(Here @a@, @b@ and @c@ are taken to be already atomic.) In particular, the
pass never duplicates the continuation into the branches, e.g. it does /not/
produce @if c then f(a) else f(b)@: doing so would blow up quadratically for
nested control flow, whereas binding at the point of use keeps the size of
the output linear in the size of the input.
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
(ANF). See the module header for the invariants and an example, and 'anfExpr'
for the traversal (including the 'ECall' special case).
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
-- Core traversal
-- ---------------------------------------------------------------------------

{- | Where a normalized expression sits in the surrounding program.

The position determines two things: whether the result must be made atomic,
and which type annotation a normalized @case@ keeps (see 'caseResultType').
-}
data Position
  = {- | A function/constant body, or a branch/clause body in tail position:
    the result is returned /in place/.
    -}
    InTail
  | -- | The RHS of a @let@-binding: like 'InTail', but see 'caseResultType'.
    InBinding
  | {- | An operand (application function/argument, operator input, record
    field/row, scrutinee, condition, or 'ECall' argument/continuation): the
    result must be atomic.
    -}
    InOperand

{- | The position in which a nested branch or clause body is normalized.

Tail position is inherited; an operand's branches are normalized in binding
position. (This mirrors the rules the pass has always applied.)
-}
nestedPosition :: Position -> Position
nestedPosition InTail = InTail
nestedPosition _ = InBinding

-- | Whether the result of the expression must be made atomic by the caller.
bindsResult :: Position -> Bool
bindsResult InOperand = True
bindsResult _ = False

{- | The result type to give a normalized @case@.

In tail position the declared annotation is preserved. In binding and operand
position the type is re-derived from the first normalized clause body — the
shape codegen unboxes with ('Coal.Kernel.LLVM.Codegen.irValue'). The two
coincide for well-typed input; both rules are preserved so that this pass's
output is unchanged.
-}
caseResultType :: Position -> Type -> NonEmpty (Clause Type) -> Type
caseResultType InTail t _ = t
caseResultType _ _ clauses = case clauses of Clause _ body :| _ -> typeOf body

{- | Normalize @expr@ in the given 'Position', delivering the result to @cont@.

Operands are normalized through a continuation, so any bindings they introduce
are hoisted to enclose the whole expression (operands must be atomic).
Branches, clause bodies and lambda bodies are normalized with an identity
continuation and placed /in position/, so their bindings stay inside the
branch/lambda rather than being hoisted out of it.

'ECall' is the one non-atomic form allowed to remain in operand position: it is
a continuation-passing foreign call whose continuation codegen applies itself
('Coal.Kernel.LLVM.Codegen.irValue'/@irTailECall@), so binding it to a fresh
variable would change the generated code. It is therefore passed straight to
@cont@.
-}
anfExpr ::
  (Monad m) =>
  Position ->
  Expr Type ->
  (Expr Type -> PipelineT m (Expr Type)) ->
  PipelineT m (Expr Type)
anfExpr pos expr cont =
  case expr of
    EVar _ -> cont expr
    ECon _ -> cont expr
    ELit _ -> cont expr
    ENil -> cont expr
    EIf cond th el ->
      anfExpr InOperand cond $ \condAtom -> do
        th' <- anfExpr (nestedPosition pos) th pure
        el' <- anfExpr (nestedPosition pos) el pure
        emit pos cont (EIf condAtom th' el')
    ECase t scrutinee clauses ->
      anfExpr InOperand scrutinee $ \scrutAtom -> do
        clauses' <- traverse (anfClause (nestedPosition pos)) clauses
        emit pos cont (ECase (caseResultType pos t clauses') scrutAtom clauses')
    ELet bs body ->
      anfLetSeq (NonEmpty.toList bs) (anfExpr pos body cont)
    ELam params body -> do
      body' <- anfExpr InTail body pure
      emit pos cont (ELam params body')
    EApp t f args ->
      anfExpr InOperand f $ \fAtom ->
        anfAll (NonEmpty.toList args) $ \argAtoms ->
          emit pos cont (EApp t fAtom (NonEmpty.fromList argAtoms))
    EOp op ->
      anfAll (opOperands op) $ \atoms ->
        emit pos cont (EOp (rebuildOp atoms op))
    EExt name e1 e2 ->
      anfExpr InOperand e1 $ \e1Atom ->
        anfExpr InOperand e2 $ \e2Atom ->
          emit pos cont (EExt name e1Atom e2Atom)
    EGet lbl e ->
      anfExpr InOperand e $ \eAtom ->
        emit pos cont (EGet lbl eAtom)
    ECall (Label t name) args k ->
      anfAll args $ \argAtoms ->
        anfExpr InOperand k $ \kAtom ->
          cont (ECall (Label t name) argAtoms kAtom)

{- | Deliver a normalized expression: bind it to a fresh @anf.N@ variable first
when the position requires an atomic operand, then pass it to @cont@.
-}
emit ::
  (Monad m) =>
  Position ->
  (Expr Type -> PipelineT m (Expr Type)) ->
  Expr Type ->
  PipelineT m (Expr Type)
emit pos cont v
  | bindsResult pos = bindFresh v cont
  | otherwise = cont v

-- | Normalize a function/constant body (tail position).
anfTail :: (Monad m) => Expr Type -> PipelineT m (Expr Type)
anfTail e = anfExpr InTail e pure

{- | Normalize a @let@-binding RHS (or clause body): control flow is kept bound
at its point of use and the surrounding computation is applied exactly once.
-}
anfLetRhs ::
  (Monad m) =>
  Expr Type ->
  (Expr Type -> PipelineT m (Expr Type)) ->
  PipelineT m (Expr Type)
anfLetRhs = anfExpr InBinding

{- | Normalize an expression in operand position and pass the atomic result to
@cont@ (see 'anfExpr' for the 'ECall' exception).
-}
anfValue ::
  (Monad m) =>
  Expr Type ->
  (Expr Type -> PipelineT m (Expr Type)) ->
  PipelineT m (Expr Type)
anfValue = anfExpr InOperand

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

-- | Normalize a clause body in the given position.
anfClause :: (Monad m) => Position -> Clause Type -> PipelineT m (Clause Type)
anfClause pos (Clause params body) = Clause params <$> anfExpr pos body pure

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
