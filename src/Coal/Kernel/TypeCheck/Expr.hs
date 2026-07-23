{-# LANGUAGE LambdaCase #-}

{- |
Expression type checking.

The core type checking logic for expressions. Traverses expressions,
accumulating type errors via a 'Writer' monad while maintaining a 'ReaderT'
environment for symbol lookup.

= Checking strategy

  * Each expression is checked against its annotated type
  * Subexpressions are checked recursively
  * Type errors are emitted but do not halt checking
  * Compatibility is determined via the 'compatible' function

= Type annotations

All expressions in the AST are annotated with types (parameter @t@). The
checker verifies that these annotations are internally consistent with the
expression structure and compatible with the surrounding context.
-}
module Coal.Kernel.TypeCheck.Expr (
  Check,
  checkExpr,
  emit,
) where

import Control.Monad (forM_, unless, when)
import Control.Monad.Reader (ReaderT, ask, asks, local)
import Control.Monad.Writer (Writer, tell)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map

import Coal.Common.Name (Name)
import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Op (Op (..))
import Coal.Kernel.Language.Type (Type (..))
import qualified Coal.Kernel.Language.Type.Constructors as Type
import Coal.Kernel.Language.Type.HasType (HasType (..), foldType, returnTypeOf, unfoldType)
import Coal.Kernel.Language.Type.Row (toNormalForm)
import Coal.Kernel.TypeCheck.Compat (compatible)
import Coal.Kernel.TypeCheck.Env (CheckEnv (..), lookupCon, lookupName, withLocals)
import Coal.Kernel.TypeCheck.Error (TypeError (..), TypeErrorKind (..))

type Check = ReaderT CheckEnv (Writer [TypeError])

-- ---------------------------------------------------------------------------
-- Error emission helpers

emit :: TypeErrorKind -> Check ()
emit kind = do
  ctx <- asks currentCtx
  tell [TypeError ctx kind]

emitMismatch :: Type -> Type -> Check ()
emitMismatch expt act = emit (TypeMismatch expt act)

-- | Emit a mismatch error when the two types are not compatible.
check :: Type -> Type -> Check ()
check expt act =
  unless (compatible expt act) (emitMismatch expt act)

-- ---------------------------------------------------------------------------
-- Main expression checker

{- | Check an expression for type consistency, returning its inferred type.
Errors are accumulated via the Writer rather than causing failure.
-}
checkExpr :: Expr Type -> Check Type
checkExpr =
  \case
    -- -----------------------------------------------------------------------
    -- Variables
    -- When a name is not found in the environment we trust the annotation.
    -- This covers built-in external functions (e.g. coal_print_int32) that are
    -- not declared via DExternal in any module.  Only names that ARE present in
    -- the environment are verified against their declared type.
    EVar (Label t name) -> do
      env <- ask
      for_ (lookupName name env) (check t)
      pure t

    -- -----------------------------------------------------------------------
    -- Constructors
    -- Built-in constructors ($Nil, $Cons, $Record, …) are not declared via
    -- DData, so we trust their annotation when no declaration is found.
    ECon (Label t name) -> do
      env <- ask
      for_ (lookupCon name env) (check t)
      pure t

    -- -----------------------------------------------------------------------
    -- Literals — no external annotation to reconcile
    ELit prim ->
      pure (typeOf prim)
    -- -----------------------------------------------------------------------
    -- Lambda abstraction
    ELam params body -> do
      let bindings = [(n, typeOf lbl) | lbl@(Label _ n) <- NonEmpty.toList params]
      bodyT <- local (withLocals bindings) (checkExpr body)
      pure (foldType bodyT (typeOf <$> params))

    -- -----------------------------------------------------------------------
    -- Let binding
    ELet bindings body -> do
      finalEnv <- checkBindings (NonEmpty.toList bindings)
      local (const finalEnv) (checkExpr body)

    -- -----------------------------------------------------------------------
    -- Function application: @<t>(f, arg1, arg2, …)
    EApp t f args -> do
      fT <- checkExpr f
      argTs <- traverse checkExpr args
      checkApp t fT (NonEmpty.toList argTs)
      pure t

    -- -----------------------------------------------------------------------
    -- Conditional
    EIf cond tr fa -> do
      condT <- checkExpr cond
      unless (compatible condT Type.bool) $
        emit (ConditionNotBool condT)
      trT <- checkExpr tr
      faT <- checkExpr fa
      unless (compatible trT faT) $
        emit (BranchTypeMismatch trT faT)
      pure trT

    -- -----------------------------------------------------------------------
    -- Operators
    EOp op -> do
      checkOp op
      pure (typeOf op)

    -- -----------------------------------------------------------------------
    -- Pattern matching: case<t>(scrutinee) { … }
    ECase t scrutinee clauses -> do
      scrutT <- checkExpr scrutinee
      forM_ clauses (checkClause t scrutT)
      pure t

    -- -----------------------------------------------------------------------
    -- Row extension: { field = val | rest }
    EExt name val rest -> do
      valT <- checkExpr val
      restT <- checkExpr rest
      -- rest must be a row-kinded type (RExt, RNil, or TOpq)
      unless (isRowType restT) $
        emit (TypeMismatch (RExt name valT RNil) restT)
      pure (RExt name valT restT)

    -- -----------------------------------------------------------------------
    -- Empty row
    ENil ->
      pure RNil
    -- -----------------------------------------------------------------------
    -- External C call: trust the annotation
    ECall (Label t _) args k -> do
      mapM_ checkExpr args
      _ <- checkExpr k
      pure t

    -- -----------------------------------------------------------------------
    -- Field projection: get?_field<t>(row)
    EGet (Label t field) row -> do
      rowT <- checkExpr row
      checkFieldAccess t field rowT
      pure t

-- ---------------------------------------------------------------------------
-- Application type checking

checkApp :: Type -> Type -> [Type] -> Check ()
checkApp resultT fT argTs =
  case fT of
    TOpq ->
      -- Opaque function: can't verify, trust the annotation
      pure ()
    _ ->
      let ts = NonEmpty.toList (unfoldType fT)
          nParams = length ts - 1
          nArgs = length argTs
       in if nArgs > nParams
            then emit (ArityMismatch nParams nArgs)
            else do
              -- Check each argument against the corresponding parameter type
              forM_ (zip argTs ts) $ \(argT, paramT) ->
                check paramT argT
              -- Check that the partial result type matches the annotation
              let resultTs = drop nArgs ts
                  partialT = case resultTs of
                    [r] -> r
                    (r : rs) -> foldType (last rs) (r : init rs)
                    [] -> fT -- shouldn't happen given nArgs <= nParams
              check resultT partialT

-- ---------------------------------------------------------------------------
-- Clause checking (pattern matching)

checkClause :: Type -> Type -> Clause Type -> Check ()
checkClause expectedT scrutT (Clause (conLabel :| fieldLabels) body) = do
  let Label conT _ = conLabel
  -- The constructor's return type must match the scrutinee type
  let conRetT = returnTypeOf conT
  check scrutT conRetT
  -- Verify constructor arity matches the number of field patterns
  let conTs = NonEmpty.toList (unfoldType conT)
      conParams = init conTs -- all but the last (return type)
      nFields = length fieldLabels
      nConParams = length conParams
  when (nFields /= nConParams) $
    emit (ArityMismatch nConParams nFields)
  -- Bind each field label and check its annotation against the constructor param
  let fieldBindings =
        [ (n, typeOf lbl)
        | lbl@(Label _ n) <- fieldLabels
        ]
  forM_ (zip fieldLabels conParams) $ \(Label fieldT _, paramT) ->
    check paramT fieldT
  bodyT <- local (withLocals fieldBindings) (checkExpr body)
  check expectedT bodyT

-- ---------------------------------------------------------------------------
-- Let binding helper

-- | Check all bindings and return an extended environment.
checkBindings :: [Binding Type] -> Check CheckEnv
checkBindings [] = ask
checkBindings (Binding (Label t n) expr : rest) = do
  exprT <- checkExpr expr
  check t exprT
  local (withLocals [(n, t)]) (checkBindings rest)

-- ---------------------------------------------------------------------------
-- Operator operand checking

checkOp :: Op (Expr Type) -> Check ()
checkOp op =
  case op of
    -- Numeric comparisons → both operands have the named numeric type
    OEqInt32 a b -> checkBinary Type.int32 a b
    OEqInt64 a b -> checkBinary Type.int64 a b
    OEqFloat a b -> checkBinary Type.float a b
    OEqDouble a b -> checkBinary Type.double a b
    OEqChar a b -> checkBinary Type.char a b
    OEqBool a b -> checkBinary Type.bool a b
    ONeInt32 a b -> checkBinary Type.int32 a b
    ONeInt64 a b -> checkBinary Type.int64 a b
    ONeFloat a b -> checkBinary Type.float a b
    ONeDouble a b -> checkBinary Type.double a b
    ONeChar a b -> checkBinary Type.char a b
    ONeBool a b -> checkBinary Type.bool a b
    OLtInt32 a b -> checkBinary Type.int32 a b
    OLtInt64 a b -> checkBinary Type.int64 a b
    OLtFloat a b -> checkBinary Type.float a b
    OLtDouble a b -> checkBinary Type.double a b
    OGtInt32 a b -> checkBinary Type.int32 a b
    OGtInt64 a b -> checkBinary Type.int64 a b
    OGtFloat a b -> checkBinary Type.float a b
    OGtDouble a b -> checkBinary Type.double a b
    OLteInt32 a b -> checkBinary Type.int32 a b
    OLteInt64 a b -> checkBinary Type.int64 a b
    OLteFloat a b -> checkBinary Type.float a b
    OLteDouble a b -> checkBinary Type.double a b
    OGteInt32 a b -> checkBinary Type.int32 a b
    OGteInt64 a b -> checkBinary Type.int64 a b
    OGteFloat a b -> checkBinary Type.float a b
    OGteDouble a b -> checkBinary Type.double a b
    -- Arithmetic → both operands and result have the same numeric type
    OAddInt32 a b -> checkBinary Type.int32 a b
    OAddInt64 a b -> checkBinary Type.int64 a b
    OAddFloat a b -> checkBinary Type.float a b
    OAddDouble a b -> checkBinary Type.double a b
    OSubInt32 a b -> checkBinary Type.int32 a b
    OSubInt64 a b -> checkBinary Type.int64 a b
    OSubFloat a b -> checkBinary Type.float a b
    OSubDouble a b -> checkBinary Type.double a b
    OMulInt32 a b -> checkBinary Type.int32 a b
    OMulInt64 a b -> checkBinary Type.int64 a b
    OMulFloat a b -> checkBinary Type.float a b
    OMulDouble a b -> checkBinary Type.double a b
    ODivInt32 a b -> checkBinary Type.int32 a b
    ODivInt64 a b -> checkBinary Type.int64 a b
    ODivFloat a b -> checkBinary Type.float a b
    ODivDouble a b -> checkBinary Type.double a b
    -- Logical
    OAnd a b -> checkBinary Type.bool a b
    OOr a b -> checkBinary Type.bool a b
    ONot a -> checkUnary Type.bool a
    -- Negation
    ONegInt32 a -> checkUnary Type.int32 a
    ONegInt64 a -> checkUnary Type.int64 a
    ONegFloat a -> checkUnary Type.float a
    ONegDouble a -> checkUnary Type.double a

checkBinary :: Type -> Expr Type -> Expr Type -> Check ()
checkBinary expected a b = do
  aT <- checkExpr a
  bT <- checkExpr b
  check expected aT
  check expected bT

checkUnary :: Type -> Expr Type -> Check ()
checkUnary expected a = do
  aT <- checkExpr a
  check expected aT

-- ---------------------------------------------------------------------------
-- Field access helpers

-- | Check that a row type contains the named field with a compatible type.
checkFieldAccess :: Type -> Name -> Type -> Check ()
checkFieldAccess fieldT field rowT =
  case rowT of
    TOpq ->
      -- Opaque row: can't verify
      pure ()
    row ->
      let (m, tail_) = toNormalForm row
       in case Map.lookup field m of
            Just found ->
              check fieldT found
            Nothing ->
              -- Field absent: OK if tail is opaque, error otherwise
              unless (isTOpq tail_) $
                emit (FieldNotFound field rowT)

-- ---------------------------------------------------------------------------
-- Predicates

isRowType :: Type -> Bool
isRowType TOpq = True
isRowType RNil = True
isRowType (RExt{}) = True
isRowType _ = False

isTOpq :: Type -> Bool
isTOpq TOpq = True
isTOpq _ = False
