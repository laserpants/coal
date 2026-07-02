{- |
Normalization pass 4: Constructor saturation.

Eta-expands every partially applied data constructor into a lambda, ensuring
that all constructor applications are fully saturated. This simplifies code
generation by eliminating the need for partial constructor closures.

= Invariant established

After this pass, every data constructor is applied to exactly the number of
arguments matching its declared arity.

= Transformation

For a constructor @C@ with @arity(C) = n@ applied to @k@ arguments:

  * @k == n@: already saturated, leave alone.
  * @k < n@: wrap in @fn(xₖ₊₁, …, xₙ) => C(e₁, …, eₖ, xₖ₊₁, …, xₙ)@.
  * @k > n@: over-saturated; throw 'OverSaturatedConstructor'.

A bare @ECon@ with @arity > 0@ is treated as @k = 0@.
-}
module Coal.Kernel.Pipeline.Pass.ConstructorSaturation (
  constructorSaturation,
) where

import Control.Monad.Error.Class (throwError)
import qualified Data.List.NonEmpty as NonEmpty

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Language.Type.Function (arity)
import Coal.Kernel.Language.Type.HasType (unfoldType)
import Coal.Kernel.Pipeline (Pass, PipelineError (..), PipelineT, freshName)
import Common (Name)

{- | Eta-expand every partially applied data constructor into a lambda.

For a constructor @C@ with @arity(C) = n@ applied to @k@ arguments:

  * @k == n@: already saturated, leave alone.
  * @k < n@: wrap in @fn(xₖ₊₁, …, xₙ) => C(e₁, …, eₖ, xₖ₊₁, …, xₙ)@.
  * @k > n@: over-saturated; throw 'OverSaturatedConstructor'.

A bare @ECon@ with @arity > 0@ is treated as @k = 0@.
-}
constructorSaturation :: (Monad m) => Pass m (Module Type) (Module Type)
constructorSaturation m = do
  objs' <- mapM saturateObject (moduleObjects m)
  pure m{moduleObjects = objs'}

-- --------------------------------------------------------------------------
-- Object
-- --------------------------------------------------------------------------

saturateObject :: (Monad m) => Object Type -> PipelineT m (Object Type)
saturateObject obj =
  case obj of
    DFunction name params body -> do
      body' <- saturateExpr body
      pure (DFunction name params body')
    DConstant name expr -> do
      expr' <- saturateExpr expr
      pure (DConstant name expr')
    DExternal{} ->
      pure obj
    DData{} ->
      pure obj

-- --------------------------------------------------------------------------
-- Expr
-- --------------------------------------------------------------------------

saturateExpr :: (Monad m) => Expr Type -> PipelineT m (Expr Type)
saturateExpr expr =
  case expr of
    EVar _ ->
      pure expr
    ECon (Label t name) ->
      -- Bare constructor with no arguments.
      saturateCon t name [] expr
    ELit _ ->
      pure expr
    ENil ->
      pure expr
    ELet bindings body -> do
      bindings' <- mapM saturateBinding bindings
      body' <- saturateExpr body
      pure (ELet bindings' body')
    ELam params body -> do
      body' <- saturateExpr body
      pure (ELam params body')
    EApp t f args -> do
      args' <- mapM saturateExpr args
      case f of
        ECon (Label conType name) -> do
          -- Constructor application: check saturation.
          saturateCon conType name (NonEmpty.toList args') (EApp t f args')
        _ -> do
          f' <- saturateExpr f
          pure (EApp t f' args')
    EIf cond th el -> do
      cond' <- saturateExpr cond
      th' <- saturateExpr th
      el' <- saturateExpr el
      pure (EIf cond' th' el')
    EOp op -> do
      op' <- mapM saturateExpr op
      pure (EOp op')
    ECase t scrutinee clauses -> do
      scrutinee' <- saturateExpr scrutinee
      clauses' <- mapM saturateClause clauses
      pure (ECase t scrutinee' clauses')
    EExt name e1 e2 -> do
      e1' <- saturateExpr e1
      e2' <- saturateExpr e2
      pure (EExt name e1' e2')
    EGet lbl e -> do
      e' <- saturateExpr e
      pure (EGet lbl e')

-- --------------------------------------------------------------------------
-- Constructor saturation helper
-- --------------------------------------------------------------------------

{- | Given a constructor type, name, and already-saturated argument list,
produce a (possibly wrapped) expression that is fully saturated.
-}
saturateCon ::
  (Monad m) =>
  -- | Type of the constructor (the full constructor type, not the result type).
  Type ->
  -- | Constructor name.
  Name ->
  -- | Already-evaluated argument expressions (k of them).
  [Expr Type] ->
  -- | The original expression (used to reconstruct saturated form when k==n).
  Expr Type ->
  PipelineT m (Expr Type)
saturateCon conType name existingArgs _original = do
  let n = arity conType
      k = length existingArgs
  case compare k n of
    EQ ->
      -- Already saturated; reconstruct cleanly.
      case NonEmpty.nonEmpty existingArgs of
        Nothing ->
          -- Nullary constructor
          pure (ECon (Label conType name))
        Just args ->
          pure (EApp (resultType conType n) (ECon (Label conType name)) args)
    LT -> do
      -- Partial application: eta-expand with (n-k) fresh params.
      -- The argument types for the missing positions come from unfolding the
      -- constructor type.
      let allArgTypes = NonEmpty.init (unfoldType conType) -- all but last = arg types
          missingArgTypes = drop k allArgTypes
      freshParams <- mapM (\t -> freshName name >>= \fresh -> pure (Label t fresh)) missingArgTypes
      let freshVars = map EVar freshParams
          saturatedArgs = existingArgs ++ freshVars
          conExpr = ECon (Label conType name)
          appType = resultType conType n
      case NonEmpty.nonEmpty saturatedArgs of
        Nothing ->
          -- n == 0, k == 0: nullary, no eta needed (unreachable given k < n)
          pure conExpr
        Just args ->
          case NonEmpty.nonEmpty freshParams of
            Nothing ->
              -- All args already present (unreachable given k < n)
              pure (EApp appType conExpr args)
            Just lamParams ->
              pure (ELam lamParams (EApp appType conExpr args))
    GT ->
      throwError (OverSaturatedConstructor name)

-- | The return type of a constructor applied to all n arguments.
resultType :: Type -> Int -> Type
resultType t n = NonEmpty.last (NonEmpty.fromList (take (n + 1) (NonEmpty.toList (unfoldType t))))

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

saturateBinding :: (Monad m) => Binding Type -> PipelineT m (Binding Type)
saturateBinding (Binding lbl e) = Binding lbl <$> saturateExpr e

saturateClause :: (Monad m) => Clause Type -> PipelineT m (Clause Type)
saturateClause (Clause params body) = Clause params <$> saturateExpr body
