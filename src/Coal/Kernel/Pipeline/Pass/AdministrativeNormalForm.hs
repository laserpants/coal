{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Normalization pass 10: Administrative normal form (ANF).

Transforms every expression in the module into administrative normal form,
where all operands are atomic and control flow appears only in tail position.
This simplifies code generation by eliminating nested complex expressions.

= Invariants established

After this pass:

  1. Every operand (function, argument, operator input, record field/row,
     scrutinee, condition) is __atomic__ (@EVar@, @ECon@, @ELit@, @ENil@).
  2. @EIf@ and @ECase@ appear only in __tail position__ — they are never the
     RHS of a @let@-binding.
  3. Non-atomic subexpressions are extracted into fresh @let@-bindings.
  4. Control flow in a non-tail position is floated outward: the surrounding
     continuation is pushed into every branch rather than wrapping the whole
     @EIf@/@ECase@ in a @let@.

= Example transformation

@
let x = if c then a else b
in body
@

becomes:

@
if c
  then let x = a in body
  else let x = b in body
@
-}
module Coal.Kernel.Pipeline.Pass.AdministrativeNormalForm (
  administrativeNormalForm,
) where

import Control.Monad.State.Strict (State, evalState, get, put)
import qualified Data.List.NonEmpty as NonEmpty

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Language.Type.HasType (typeOf)
import Coal.Kernel.Pipeline (Pass, PipelineT, freshName)

{- | Transform every expression in the module into administrative normal form
(ANF).

= Invariants

After this pass:

  1. Every operand (function, argument, operator input, record field/row,
     scrutinee, condition) is __atomic__ (@EVar@, @ECon@, @ELit@, @ENil@).
  2. @EIf@ and @ECase@ appear only in __tail position__ — they are never the
     RHS of a @let@-binding.
  3. Non-atomic subexpressions are extracted into fresh @let@-bindings.
  4. Control flow in a non-tail position is floated outward: the surrounding
     continuation is pushed into every branch rather than wrapping the whole
     @EIf@/@ECase@ in a @let@.

= Example

@
let x = if c then a else b
in body
@

becomes:

@
if c
  then let x = a in body
  else let x = b in body
@
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
    DFunction name params body ->
      DFunction name params <$> anfTail body
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
atomized and their branches are normalized tail-recursively. Any @let@
in the binding sequence whose RHS is control flow is floated outward via
'anfLetSeq'.
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

-- --------------------------------------------------------------------------
-- Operand-position (let-binding RHS) normalization
-- ---------------------------------------------------------------------------
-- Value-position normalization (continuation-passing)
-- ---------------------------------------------------------------------------

{- | Normalize @expr@ in value (operand) position and pass the /atomic/
result to @cont@.

When @expr@ is @EIf@ or @ECase@, the continuation is pushed into every
branch so that no control-flow expression appears as a @let@-binding RHS:

@
  anfValue (if c then a else b) cont
    ≡  if c_atom then (anfValue a cont) else (anfValue b cont)
@

For all other non-atomic expressions sub-expressions are atomized
(possibly floating control flow from them outward) and the result is bound
to a fresh name via 'bindFresh'.
-}
anfValue ::
  (Monad m) =>
  Expr Type ->
  (Expr Type -> PipelineT m (Expr Type)) ->
  PipelineT m (Expr Type)
anfValue expr cont
  | isAtomic expr = cont expr
  | otherwise = case expr of
      EIf cond th el ->
        anfValue cond $ \condAtom -> do
          th' <- anfValue th cont
          el' <- anfValue el cont
          pure (EIf condAtom th' el')
      ECase _t scrutinee clauses ->
        anfValue scrutinee $ \scrutAtom -> do
          clauses' <- mapM (\(Clause ps b) -> Clause ps <$> anfValue b cont) clauses
          -- The continuation changes the branch type; derive the new case
          -- result type from the transformed first branch rather than reusing
          -- the now-stale pre-continuation type.
          let newT = let Clause _ body = NonEmpty.head clauses' in typeOf body
          pure (ECase newT scrutAtom clauses')
      ELet bs body ->
        anfLetSeq (NonEmpty.toList bs) (anfValue body cont)
      ELam params body -> do
        body' <- anfTail body
        bindFresh (ELam params body') cont
      _ ->
        anfLetRhs expr $ \expr' ->
          bindFresh expr' cont

-- ---------------------------------------------------------------------------
-- Let-binding RHS normalization
-- ---------------------------------------------------------------------------

{- | Normalize @expr@ for use as the RHS of a @let@-binding.

The continuation @cont@ receives a non-control-flow, ANF'd value
expression. If @expr@ is @EIf@ or @ECase@ the control flow is floated
outward by pushing @cont@ into every branch.
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
          th' <- anfLetRhs th cont
          el' <- anfLetRhs el cont
          pure (EIf condAtom th' el')
      ECase _t scrutinee clauses ->
        anfValue scrutinee $ \scrutAtom -> do
          clauses' <- mapM (\(Clause ps b) -> Clause ps <$> anfLetRhs b cont) clauses
          let newT = let Clause _ body = NonEmpty.head clauses' in typeOf body
          pure (ECase newT scrutAtom clauses')
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
      other -> cont other

-- ---------------------------------------------------------------------------
-- Let-binding sequence processing
-- ---------------------------------------------------------------------------

{- | Process a sequence of @let@-bindings followed by a body.

Each binding RHS is normalized via 'anfLetRhs'. If a RHS is control flow
it is floated outward: the remaining bindings and the body computation are
pushed into every branch.
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
through to the continuation. Control flow inside any element is floated
outward.
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

-- | Extract the operands of an 'Op' as a list.
opOperands :: (Foldable f) => f a -> [a]
opOperands = foldr (:) []

-- | Rebuild a 'Traversable' functor by popping from a replacement list.
rebuildOp :: (Traversable f) => [a] -> f b -> f a
rebuildOp xs op = evalState (traverse (const pop) op) xs
 where
  pop :: State [a] a
  pop = do
    xs' <- get
    put (tail xs')
    pure (head xs')
