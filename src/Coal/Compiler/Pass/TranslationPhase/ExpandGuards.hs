{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Pass.TranslationPhase.ExpandGuards

Expand guard expressions in pattern matching clauses into nested if-expressions.

This pass transforms pattern matching clauses that use guard expressions into
explicit if-then-else expressions. Guards provide a convenient way to add
conditions to pattern matches, and this pass desugars them into the underlying
conditional expressions.

For example, a guarded clause like:

@
match(x) {
  | Just(y)
      when (y > 0) => positive(y)
      when (y < 0) => negative(y)
  | Nothing =>
      Zero
}
@

is expanded into:

@
match(x) {
  | Just(y) =>
      if (y > 0) then positive(y)
        else if (y < 0) then negative(y)
        else match(x) { | Nothing => Zero }
  | Nothing => Zero
}
@

Multiple guards within a single clause are combined with logical AND, and
the expansion ensures proper fallthrough to subsequent clauses when guards
fail. This transformation makes the control flow explicit for later compiler
stages.
-}
module Coal.Compiler.Pass.TranslationPhase.ExpandGuards (
  passExpandGuards,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language
import Coal.Language.Definition
import Coal.Language.Module (Module (..))
import Data.Foldable (foldrM)
import Data.Generics.Uniplate.Data (transformBiM)
import Data.List.NonEmpty (NonEmpty (..), tails)
import qualified Data.List.NonEmpty as NonEmpty

{- | Guard expansion pass.

Transform pattern matching clauses with guard expressions into explicit
if-then-else expressions. Combine multiple guards with logical AND and ensure
proper fallthrough to subsequent clauses when guards fail, making control flow
explicit for later compilation stages.
-}
passExpandGuards :: (Monad m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passExpandGuards = Pass{runPass = expandModule}

-- | Check if a clause is trivial (has no guards)
trivial :: Clause a Kind t -> Bool
trivial (EClause _ _ (CPlain _ [] _ :| [])) = True
trivial _ = False

-- | Expand guard expressions within a single expression
expandExpression :: (Monad m) => Expression Metadata Kind IndexedType -> CompilerT Metadata m (Expression Metadata Kind IndexedType)
expandExpression =
  \case
    e@(EMatch _ _ _ cs)
      | all trivial cs ->
          return e
    EMatch a t e cs -> do
      expr <- expandExpression e
      name <- supplied (freshName "scr")
      let ll = Label (typeOf e) name
          var = EVariable a ll
      cs' <- traverse (expandClauseGuards a t var) (NonEmpty.init $ tails cs)
      case cs' of
        x : xs ->
          return $
            ELet
              a
              (BPattern a (PVariable a ll) expr :| [])
              (EMatch a t var (x :| xs))
        [] ->
          error "Implementation error"
    e ->
      return e

-- | Expand guards in all definitions within a module
expandModule :: (Monad m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
expandModule =
  \case
    Module{..} -> do
      newDefinitions <- traverse expandDefinition moduleDefinitions
      return $
        Module
          { moduleDefinitions = newDefinitions
          , ..
          }

-- | Expand guards in a single definition
expandDefinition :: (Monad m) => Definition Metadata Kind IndexedType -> CompilerT Metadata m (Definition Metadata Kind IndexedType)
expandDefinition =
  \case
    DFunction loc name def ->
      DFunction loc name <$> transformBiM expandExpression def
    DLet loc name def ->
      DLet loc name <$> transformBiM expandExpression def
    DInstance loc InstanceDefinition{..} -> do
      newImplementations <- traverse expandDefinition instanceDefinitionImplementations
      return $
        DInstance
          loc
          InstanceDefinition
            { instanceDefinitionImplementations = newImplementations
            , ..
            }
    d ->
      return d

{- | Expand guards in a clause, with fallback to remaining clauses

This transforms a clause with guards into nested if-expressions.
For example, a clause like:
  | guard1 -> expr1
  | guard2 -> expr2
becomes:
  if guard1 then expr1 else if guard2 then expr2 else <fallback>
-}
expandClauseGuards :: (Monad m) => Metadata -> IndexedType -> Expression Metadata Kind IndexedType -> [Clause Metadata Kind IndexedType] -> CompilerT Metadata m (Clause Metadata Kind IndexedType)
expandClauseGuards _ _ _ (trivialClause@EClause{clauseChoices = CPlain _ [] _ :| []} : _) =
  return trivialClause
expandClauseGuards loc clauseType scrutinee (EClause{..} : remainingClauses) = do
  let fallbackClause = buildFallbackClause remainingClauses
      fallbackMatch = EMatch loc clauseType scrutinee (NonEmpty.fromList $ remainingClauses <> [fallbackClause])
  nextExpr <- expandExpression fallbackMatch
  expanded <- foldrM buildGuardedIf nextExpr clauseChoices

  return $ EClause clauseMetadata clausePattern (CPlain loc [] expanded :| [])
 where
  buildFallbackClause clauses =
    let EClause
          { clauseMetadata = fallbackMeta
          , clausePattern = fallbackPattern
          , clauseChoices = fallbackChoices
          } = last clauses
     in EClause fallbackMeta (PAny fallbackMeta (typeOf fallbackPattern)) fallbackChoices

  buildGuardedIf (CPlain loc1 guards expr) elseExpr =
    return $ EIf loc1 (typeOf elseExpr) combinedGuard expr elseExpr
   where
    combinedGuard = foldr1 (conjunction loc1) (guardExpression <$> guards)
expandClauseGuards _ _ _ _ =
  error "Implementation error: expandClauseGuards called with invalid arguments"

-- | Combine two boolean expressions with logical AND
conjunction :: Metadata -> Expression Metadata Kind IndexedType -> Expression Metadata Kind IndexedType -> Expression Metadata Kind IndexedType
conjunction a e1 e2 = EApplication a (TIntrinsic IBool) e1 (e2 :| [])
