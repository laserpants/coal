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
   become the else-branch to ensure proper pattern matching semantics.

5. **Type-specific conversion**: Integers are converted using appropriate constructors
   (`from_int32`, `from_int64`, or `from_bignum`) based on their magnitude.

This pass runs during the translation phase after type checking and before
further lowering transformations.
-}
module Coal.Compiler.Pass.PhaseTranslation.ExpandIntegerLiteralPatterns (
  passExpandIntegerLiteralPatterns,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (supplied)
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Compiler.State (CompilerState (compilerCurrentPath))
import Coal.Language
import Coal.Language.Definition
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (principalPath)
import Control.Monad.Except (throwError)
import Control.Monad.State (gets)
import Control.Monad.Writer (MonadTrans (lift), MonadWriter (tell), WriterT (runWriterT))
import qualified Data.ByteString.Char8 as ByteString
import Data.Data (Data)
import Data.Generics.Uniplate.Data (descendM, transformM)
import Data.List (tails)
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
      EMatch a t e cs ->
        EMatch a t e . NonEmpty.fromList <$> traverse (expandClause a e) pairs
       where
        pairs = [(p, ps) | (p : ps) <- tails (NonEmpty.toList cs)]
      e ->
        descendM expandIntegerLiteralPatterns e

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
