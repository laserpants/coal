{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Pass.PhaseTranslation.ExpandIntegerLiteralPatterns
Description: Expansion of integer literal patterns into equality guards

This module implements the desugaring of integer literals in pattern matching.
Integer literal patterns at the source level are lowered into explicit equality
checks, as the underlying pattern match compiler operates on constructor
discrimination. This pass bridges the gap between high-level literal patterns
and the core pattern matching implementation.

Key transformations:

1. **Integer pattern conversion**: A pattern like `| 42 => expr` is transformed
   into a fresh variable pattern with an if-expression that checks equality.

2. **Fresh variable generation**: Each integer literal pattern is replaced with
   a fresh variable, and the integer value is recorded for generating the equality check.

3. **Guard synthesis**: All integer literals in a pattern are combined into a
   single boolean expression using logical AND operations.

4. **Fallthrough handling**: When integer patterns are present, subsequent clauses
   become the else-branch to ensure proper pattern matching semantics. Clauses are
   processed right-to-left and each fallthrough match on a suffix of the clause
   list is constructed exactly once (see `expandClausesFallthrough`), so the
   output size is linear in the number of clauses. The match scrutinee is bound
   once to a fresh variable before clause expansion, so fallthrough re-matches
   that variable rather than re-evaluating an effectful scrutinee expression.

5. **Type-specific conversion**: Integers are converted using appropriate constructors
   (`from_int32`, `from_int64`, or `from_bignum`) based on their magnitude.

This pass runs during the translation phase after type checking and before
further lowering transformations.
-}
module Coal.Compiler.Pass.PhaseTranslation.ExpandIntegerLiteralPatterns (
  passExpandIntegerLiteralPatterns,
) where

import Coal.Common.Label (Label (..))
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Compiler.State (CompilerState (compilerCurrentPath))
import Coal.Language
import Coal.Language.Module.Path (principalPath)
import Control.Monad.Except (throwError)
import Control.Monad.State (gets)
import Control.Monad.Writer (MonadTrans (lift), MonadWriter (tell), WriterT (runWriterT))
import qualified Data.ByteString.Char8 as ByteString
import Data.Data (Data)
import Data.Generics.Uniplate.Data (descendM, transformM, universe)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import GHC.Int (Int32, Int64)
import TextShow (showt)

passExpandIntegerLiteralPatterns :: (Monad m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passExpandIntegerLiteralPatterns = Pass{runPass = passImpl}

passImpl :: (Monad m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
passImpl = expandIntegerLiteralPatterns

-- | Types that can have integer literal patterns expanded
class ExpandContext e where
  expandIntegerLiteralPatterns :: (Monad m) => e -> CompilerT Metadata m e

instance (ExpandContext e) => ExpandContext [e] where
  expandIntegerLiteralPatterns = traverse expandIntegerLiteralPatterns

instance (ExpandContext e) => ExpandContext (NonEmpty e) where
  expandIntegerLiteralPatterns = traverse expandIntegerLiteralPatterns

instance (Data k) => ExpandContext (Expression Metadata k IndexedType) where
  expandIntegerLiteralPatterns =
    \case
      -- When a match contains integer literal patterns, fallthrough arms rebuild
      -- a nested match on the scrutinee. Bind the scrutinee once first so an
      -- effectful scrutinee (e.g. EventSource.select) is not re-evaluated on
      -- every failed arm. Matches without integer patterns are left unchanged
      -- aside from recursive expansion of sub-expressions.
      EMatch a t e cs -> do
        e' <- expandIntegerLiteralPatterns e
        let clauseList = NonEmpty.toList cs
        if any clauseHasIntegerLiteral cs
          then do
            scrutName <- supplied (freshName "scrut")
            let scrutType = typeOf e'
                scrutLabel = Label scrutType scrutName
                scrutVar = EVariable mempty scrutLabel
            cs' <- expandClausesFallthrough scrutVar clauseList
            pure $
              ELet
                mempty
                (BPattern mempty (PVariable mempty scrutLabel) e' :| [])
                (EMatch a t scrutVar (NonEmpty.fromList cs'))
          else do
            cs' <- NonEmpty.fromList <$> traverse (\c -> expandClause a e' (c, [])) clauseList
            pure (EMatch a t e' cs')
      e ->
        descendM expandIntegerLiteralPatterns e

-- | True if any pattern in the clause contains an integer literal sub-pattern.
clauseHasIntegerLiteral :: (Data k) => Clause Metadata k IndexedType -> Bool
clauseHasIntegerLiteral (EClause _ p _) = any isIntegerLiteral (universe p)
 where
  isIntegerLiteral = \case
    PInteger{} -> True
    _ -> False

{- | Expand a single clause, converting integer patterns to guards.
Takes the clause and remaining clauses (for fallthrough), and produces
a clause with fresh variables and an if-expression guard.
-}
expandClause :: (Monad m, Data k) => Metadata -> Expression Metadata k IndexedType -> (Clause Metadata k IndexedType, [Clause Metadata k IndexedType]) -> CompilerT Metadata m (Clause Metadata k IndexedType)
expandClause _ expr (EClause a p (CPlain a1 gs e1 :| []), ds) = do
  e1' <- expandIntegerLiteralPatterns e1
  (q, ints) <- runWriterT (transformM collectIntegerLiteralPatterns p)
  case (ints, ds) of
    ([], _) ->
      return (EClause a p (CPlain a1 gs e1' :| []))
    (_, []) -> do
      path <- gets compilerCurrentPath
      tellErrors [NonExhaustivePatterns (ErrorLocation (principalPath path) a)]
      throwError PatternAnomaly
    (_, c : cs) -> do
      e2 <-
        expandIntegerLiteralPatterns $
          EIf
            mempty
            (typeOf e1')
            (foldr numericLiteral (ELiteral mempty (LBool True)) ints)
            e1'
            (EMatch mempty (typeOf e1') expr (c :| cs))
      return (EClause a q (CPlain a1 gs e2 :| []))
expandClause _ _ _ = error "expandClause: expected single plain clause body"

{- | Expand a clause list so that integer literal patterns become equality
guards. Clauses are processed right-to-left and each fallthrough match on a
suffix of the clause list is constructed exactly once, so the output size is
linear in the number of clauses. (Re-expanding every suffix into every
preceding clause makes the output exponential in the number of
integer-literal clauses.)

A clause whose pattern becomes a fresh variable matches unconditionally, so
the clauses after it in the enclosing match are unreachable and are omitted;
the fallthrough lives in the body's nested match. A clause with a constructor
pattern that contains integer sub-patterns keeps its pattern (so it remains
reachable in the enclosing match) and additionally embeds the fallthrough
match for the guard-failure case.
-}
expandClausesFallthrough ::
  (Monad m, Data k) =>
  Expression Metadata k IndexedType ->
  [Clause Metadata k IndexedType] ->
  CompilerT Metadata m [Clause Metadata k IndexedType]
expandClausesFallthrough _ [] = pure []
expandClausesFallthrough scrutVar (EClause a p (CPlain a1 gs e1 :| []) : cs) = do
  cs' <- expandClausesFallthrough scrutVar cs
  e1' <- expandIntegerLiteralPatterns e1
  (q, ints) <- runWriterT (transformM collectIntegerLiteralPatterns p)
  case ints of
    -- No integer literals in this clause: keep the pattern so the clause
    -- remains reachable in the enclosing match, and keep the expanded
    -- remaining clauses after it.
    [] ->
      pure (EClause a p (CPlain a1 gs e1' :| []) : cs')
    _ | null cs -> do
      path <- gets compilerCurrentPath
      tellErrors [NonExhaustivePatterns (ErrorLocation (principalPath path) a)]
      throwError PatternAnomaly
    _ -> do
      let fallthrough =
            EMatch
              mempty
              (typeOf e1')
              scrutVar
              (NonEmpty.fromList cs')
          e2 =
            EIf
              mempty
              (typeOf e1')
              (foldr numericLiteral (ELiteral mempty (LBool True)) ints)
              e1'
              fallthrough
          clause' = EClause a q (CPlain a1 gs e2 :| [])
      case q of
        -- The pattern was a top-level integer literal, now a fresh variable:
        -- it matches unconditionally, so the remaining clauses are
        -- unreachable in the enclosing match.
        PVariable{} -> pure [clause']
        _ -> pure (clause' : cs')
expandClausesFallthrough _ _ = error "expandClausesFallthrough: expected single plain clause body"

-- | Create an equality check for a numeric literal variable
numericLiteral :: (Label IndexedType, Integer) -> Expression Metadata k IndexedType -> Expression Metadata k IndexedType
numericLiteral (ll@(Label t _), int) e1 =
  EApplication
    mempty
    (TIntrinsic IBool)
    (EOperator mempty (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalAnd)
    ( e1
        :| [ EApplication
               mempty
               (TIntrinsic IBool)
               (EVariable mempty (Label (t `TArrow` t `TArrow` TIntrinsic IBool) "(==)"))
               (EVariable mempty ll :| [fromLiteral t int])
           ]
    )

{- | Convert an integer literal to an expression using the appropriate constructor.
Chooses between from_int32, from_int64, or from_bignum based on the value.
-}
fromLiteral :: IndexedType -> Integer -> Expression Metadata k IndexedType
fromLiteral t int
  | int <= fromIntegral (maxBound :: Int32) =
      EApplication mempty t (EVariable mempty (Label (TIntrinsic IInt32 `TArrow` t) "from_int32")) (ELiteral mempty (LInt32 (fromIntegral int)) :| [])
  | int <= fromIntegral (maxBound :: Int64) =
      EApplication mempty t (EVariable mempty (Label (TIntrinsic IInt64 `TArrow` t) "from_int64")) (ELiteral mempty (LInt64 (fromIntegral int)) :| [])
  | otherwise =
      EApplication
        mempty
        t
        (EVariable mempty (Label (TIntrinsic IString `TArrow` t) "from_bignum"))
        ( EApplication
            mempty
            (TIntrinsic IBignum)
            (EVariable mempty (Label (TIntrinsic IString `TArrow` t) "number$_unsafe_parse_bignum"))
            (ELiteral mempty (LString (ByteString.pack $ show int)) :| [])
            :| []
        )

{- | Collect all integer literal patterns in a pattern, replacing them with fresh variables.
Returns the transformed pattern and a list of (variable, integer) pairs.
-}
collectIntegerLiteralPatterns :: (Monad m) => Pattern Metadata k IndexedType -> WriterT [(Label IndexedType, Integer)] (CompilerT Metadata m) (Pattern Metadata k IndexedType)
collectIntegerLiteralPatterns =
  \case
    PInteger a t int -> do
      n <- lift (supplied id)
      let ll = Label t ("int" <> ".[" <> showt n <> "]")
      tell [(ll, int)]
      return (PVariable a ll)
    p ->
      return p

instance ExpandContext (FunctionDefinition Metadata Kind IndexedType) where
  expandIntegerLiteralPatterns =
    \case
      FunctionDefinition{..} -> do
        newFunctionDefinitionExpression <- expandIntegerLiteralPatterns functionDefinitionExpression
        return $
          FunctionDefinition
            { functionDefinitionExpression = newFunctionDefinitionExpression
            , ..
            }

instance ExpandContext (LetDefinition Metadata Kind IndexedType) where
  expandIntegerLiteralPatterns =
    \case
      LetDefinition{..} -> do
        newLetDefinitionExpression <- expandIntegerLiteralPatterns letDefinitionExpression
        return $
          LetDefinition
            { letDefinitionExpression = newLetDefinitionExpression
            , ..
            }

instance ExpandContext (Definition Metadata Kind IndexedType) where
  expandIntegerLiteralPatterns =
    \case
      DFunction a name def ->
        DFunction a name <$> expandIntegerLiteralPatterns def
      DLet a name def ->
        DLet a name <$> expandIntegerLiteralPatterns def
      d ->
        return d

instance ExpandContext (Module Metadata Kind IndexedType) where
  expandIntegerLiteralPatterns =
    \case
      Module{..} -> do
        setCurrentPathC modulePath
        newModuleDefinitions <- traverse expandIntegerLiteralPatterns moduleDefinitions
        return $
          Module
            { moduleDefinitions = newModuleDefinitions
            , ..
            }
