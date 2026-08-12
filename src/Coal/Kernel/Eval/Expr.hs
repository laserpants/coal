{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Expression evaluation.

The interpreter for Coal kernel language expressions. Uses environment-based
call-by-value semantics.

= Evaluation strategy

  * Variables are looked up in the environment
  * Constructors are evaluated to partially applied constructor values
  * Functions are evaluated to closures capturing their free variables
  * Application performs pattern matching and substitution
  * Primitives and operators delegate to specialized handlers

= Monad stack

Evaluation runs in 'EvalM', which combines 'ExceptT' for error handling with
'ReaderT' for the environment and 'IO' for external effects.
-}
module Coal.Kernel.Eval.Expr (
  eval,
  apply,
) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map

import Coal.Common.Name (Name)
import Coal.Kernel.Eval.External (callExtern)
import Coal.Kernel.Eval.Pattern (matchClause)
import Coal.Kernel.Eval.Prim (evalOp, evalPrim)
import Coal.Kernel.Eval.State (
  EvalEnv (..),
  EvalError (..),
  EvalM,
  askEnv,
  extendEnvMany,
  lookupVar,
  throwEval,
 )
import Coal.Kernel.Eval.Value (Closure (..), Value (..))
import Coal.Kernel.Language.Expr (
  Binding (..),
  Clause (..),
  Expr (..),
  Label (..),
 )
import Coal.Kernel.Language.Type (Type)

-- ---------------------------------------------------------------------------
-- Expression evaluator
-- ---------------------------------------------------------------------------

eval :: Expr Type -> EvalM Value
eval =
  \case
    -- Literal primitive
    ELit prim ->
      return (evalPrim prim)
    -- Variable reference: look up in current environment
    EVar (Label _ name) ->
      lookupVar name
    -- Constructor reference: produce a zero-arity or partial constructor value
    ECon (Label _ name) ->
      -- We represent unapplied constructors as closures with 0 captured params.
      -- The index (-1 sentinel) will be overwritten when the constructor is
      -- applied and the DData table is consulted via the environment.
      --
      -- For use in case-matching, the constructor name is what matters.
      return (VConstructor name 0 [])
    -- Lambda: capture the current environment
    ELam params body -> do
      env <- askEnv
      return
        ( VClosure
            Closure
              { closureName = "<lambda>"
              , closureParams = NonEmpty.toList params
              , closureBody = body
              , closureEnv = closureEnvFromEvalEnv env
              }
        )
    -- Let bindings: evaluate each binding in sequence, threading the new names
    ELet bindings body -> do
      -- Use letrec-style: add all names to the env first so they can be mutually
      -- recursive (functions only; cyclic value bindings are not detected).
      evalLetBindings (NonEmpty.toList bindings) (eval body)
    -- If-then-else
    EIf cond thenExpr elseExpr -> do
      cv <- eval cond
      case cv of
        VBool True ->
          eval thenExpr
        VBool False ->
          eval elseExpr
        other ->
          throwEval (TypeMismatch "VBool" (describeValueKind other))
    -- Application: evaluate function and arguments, then apply
    EApp _ fn args -> do
      fv <- eval fn
      avs <- traverse eval args
      apply fv (NonEmpty.toList avs)
    -- Operators: evaluate operands, then dispatch
    EOp op -> do
      op' <- traverse eval op
      evalOp op'
    -- Case: evaluate scrutinee, find matching clause, evaluate its body
    ECase _ scrutineeExpr clauses -> do
      sv <- eval scrutineeExpr
      (bindings, Clause _ body) <- matchClause sv clauses
      extendEnvMany bindings (eval body)
    -- Empty record
    ENil -> return (VRecord Map.empty)
    -- Record extension: evaluate both value and rest, then extend
    EExt fieldName valExpr restExpr -> do
      vv <- eval valExpr
      rv <- eval restExpr
      case rv of
        VRecord m ->
          return (VRecord (Map.insert fieldName vv m))
        other ->
          throwEval (TypeMismatch "VRecord" (describeValueKind other))
    -- External C call: evaluate arguments, delegate to extern handler
    ECall (Label _ name) args k -> do
      avs <- traverse eval args
      r <- callExtern name avs
      kv <- eval k
      apply kv [r]
    -- Field projection: evaluate the row, return the named field value directly.
    EGet (Label _ fieldName) rowExpr -> do
      rowVal <- eval rowExpr
      let innerRow = case rowVal of
            VConstructor "$Record" _ [VRecord m] -> VRecord m
            other -> other
      case innerRow of
        VRecord m ->
          case Map.lookup fieldName m of
            Nothing ->
              throwEval
                ( PatternMatchFailure
                    ("Field not found in record: " <> show fieldName)
                )
            Just fieldVal -> return fieldVal
        other ->
          throwEval (TypeMismatch "VRecord" (describeValueKind other))

-- ---------------------------------------------------------------------------
-- Function application
-- ---------------------------------------------------------------------------

{- | Apply a value (which should be a closure or a partially-applied constructor)
to a list of argument values.
-}
apply :: Value -> [Value] -> EvalM Value
apply fv [] = return fv
apply fv args =
  case fv of
    VClosure closure ->
      applyToClosure closure args
    -- A constructor applied to arguments: accumulate fields.
    VConstructor name idx existing ->
      return (VConstructor name idx (existing <> args))
    -- An external function: delegate to the extern table.
    VExtern name ->
      callExtern name args
    other ->
      throwEval
        ( TypeMismatch
            "VClosure or VConstructor"
            (describeValueKind other)
        )

applyToClosure :: Closure -> [Value] -> EvalM Value
applyToClosure closure args =
  let remaining = closureParams closure
      needed = length remaining
      given = length args
   in if given < needed
        then
          return $
            -- Partial application: consume the supplied args, return new closure
            VClosure
              closure
                { closureParams = drop given remaining
                , closureEnv =
                    Map.fromList (zip (labelName <$> take given remaining) args)
                      `Map.union` closureEnv closure
                }
        else do
          -- Saturate: bind all params and evaluate body
          let paramBindings = zip (labelName <$> remaining) (take needed args)
              capturedBindings = Map.toList (closureEnv closure)
              allBindings = capturedBindings <> paramBindings
              extraArgs = drop needed args
          result <- extendEnvMany allBindings (eval (closureBody closure))
          -- Over-saturation: apply remaining args to the result
          apply result extraArgs

labelName :: Label t -> Name
labelName (Label _ n) = n

-- ---------------------------------------------------------------------------
-- Letrec-style let-binding evaluation
-- ---------------------------------------------------------------------------

evalLetBindings :: [Binding Type] -> EvalM Value -> EvalM Value
evalLetBindings [] cont = cont
evalLetBindings (Binding (Label _ name) rhs : rest) cont = do
  v <- eval rhs
  extendEnvMany [(name, v)] (evalLetBindings rest cont)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

describeValueKind :: Value -> String
describeValueKind =
  \case
    VUnit ->
      "VUnit"
    VBool{} ->
      "VBool"
    VInt32{} ->
      "VInt32"
    VInt64{} ->
      "VInt64"
    VBignum{} ->
      "VBignum"
    VFloat{} ->
      "VFloat"
    VDouble{} ->
      "VDouble"
    VChar{} ->
      "VChar"
    VString{} ->
      "VString"
    VConstructor{} ->
      "VConstructor"
    VRecord{} ->
      "VRecord"
    VClosure{} ->
      "VClosure"
    VExtern{} ->
      "VExtern"

closureEnvFromEvalEnv :: EvalEnv -> Map.Map Name Value
closureEnvFromEvalEnv = envBindings
