{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Pass.PhaseTranslation.DesugarPatterns

Desugar complex patterns into simple variable patterns with explicit match expressions.

This pass simplifies pattern matching by transforming complex patterns in
bindings, lambdas, and let-expressions into simple variable patterns,
extracting the pattern matching logic into explicit match expressions.

For example, a let binding with a complex pattern:

@
let (x, y) = tuple in body
@

is desugared into:

@
let v = tuple in match(v) {
  | (x, y) => body
}
@

Similarly, function patterns are extracted:

@
fn(Just(x)) => x + 1
@

becomes:

@
fn(v) =>
  match(v) {
    | Just(x) => x + 1
  }
@

This transformation normalizes the AST by ensuring that only simple variable
patterns appear in bindings, with all structural pattern matching performed
through explicit match expressions.
-}
module Coal.Compiler.Pass.PhaseTranslation.DesugarPatterns (
  passDesugarPatterns,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Journal (listenPatterns, tellPatterns)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language (IndexedType, Kind (..))
import Coal.Language.Definition
import Coal.Language.Expression (Clause (..), Expression (..))
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Expression.Choice (Choice (..))
import Coal.Language.HasType (HasType (..), foldTypeOf)
import Coal.Language.Module (Module (..))
import Coal.Language.Pattern (Pattern (..))
import Data.Generics.Uniplate.Data (descendM)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Extras (Name)

{- | Pattern desugaring pass.

Transform complex patterns in bindings, lambdas, and let-expressions into
simple variable patterns with explicit match expressions. This normalization
ensures that only trivial variable patterns appear in bindings, while all
structural pattern matching is performed through explicit match constructs.
-}
passDesugarPatterns :: (Monad m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passDesugarPatterns = Pass{runPass = passImpl}

passImpl :: (Monad m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
passImpl = desugarPatterns

class PatternContext s where
  desugarPatterns :: (Monad m) => s -> CompilerT Metadata m s

instance PatternContext (Pattern Metadata Kind IndexedType) where
  desugarPatterns =
    \case
      p@PVariable{} ->
        pure p
      p@(PAnnotation _ _ PVariable{}) ->
        pure p
      PShorthand loc (Label t name) ->
        desugarPatterns (PVariable loc (Label t name))
      p -> do
        name <- supplied (freshName "v")
        tellPatterns [(name, p)]
        pure (PVariable mempty (Label (typeOf p) name))

instance PatternContext (Binding Expression Metadata Kind IndexedType) where
  desugarPatterns =
    \case
      BPattern a p e ->
        BPattern a <$> desugarPatterns p <*> desugarPatterns e
      BFunction a name ps e ->
        desugarPatterns
          ( BPattern
              a
              (PVariable mempty (Label (foldTypeOf e ps) name))
              (ELambda mempty ps e)
          )

instance PatternContext (Expression Metadata Kind IndexedType) where
  desugarPatterns = go
   where
    go =
      \case
        ELet a gs e1 -> do
          d1 <- desugarPatterns e1
          (hs, ps) <- listenPatterns (traverse desugarPatterns gs)
          pure (ELet a hs (foldr (unrollMatch a) d1 ps))
        ERecursiveLet a p e1 e2 -> do
          d1 <- desugarPatterns e1
          d2 <- desugarPatterns e2
          (q, ps) <- listenPatterns (desugarPatterns p)
          pure (ERecursiveLet a q d1 (foldr (unrollMatch a) d2 ps))
        ELambda a ps e -> do
          e1 <- desugarPatterns e
          (qs, rs) <- listenPatterns (traverse desugarPatterns ps)
          pure (ELambda a qs (foldr (unrollMatch a) e1 rs))
        e ->
          descendM go e

unrollMatch :: Metadata -> (Name, Pattern Metadata Kind IndexedType) -> Expression Metadata Kind IndexedType -> Expression Metadata Kind IndexedType
unrollMatch loc (name, p) e =
  EMatch
    loc
    (typeOf e)
    (EVariable mempty (Label (typeOf p) name))
    (EClause loc p (CPlain mempty [] e :| []) :| [])

instance PatternContext (Module Metadata Kind IndexedType) where
  desugarPatterns =
    \case
      Module{..} -> do
        newModuleDefinitions <- traverse desugarPatterns moduleDefinitions
        pure
          Module
            { moduleDefinitions = newModuleDefinitions
            , ..
            }

instance PatternContext (Definition Metadata Kind IndexedType) where
  desugarPatterns =
    \case
      DFunction loc name def ->
        DFunction loc name <$> desugarPatterns def
      DLet loc name def ->
        DLet loc name <$> desugarPatterns def
      DInstance loc InstanceDefinition{..} -> do
        newInstanceDefinitionImplementations <- traverse desugarPatterns instanceDefinitionImplementations
        pure $
          DInstance
            loc
            InstanceDefinition
              { instanceDefinitionImplementations = newInstanceDefinitionImplementations
              , ..
              }
      d ->
        pure d

instance PatternContext (FunctionDefinition Metadata Kind IndexedType) where
  desugarPatterns =
    \case
      FunctionDefinition{..} -> do
        newFunctionDefinitionExpression <- desugarPatterns functionDefinitionExpression
        (qs, rs) <- listenPatterns (traverse desugarPatterns functionDefinitionPatterns)
        pure $
          FunctionDefinition
            { functionDefinitionPatterns =
                qs
            , functionDefinitionExpression =
                foldr (unrollMatch functionDefinitionMetadata) newFunctionDefinitionExpression rs
            , ..
            }

instance PatternContext (LetDefinition Metadata Kind IndexedType) where
  desugarPatterns =
    \case
      LetDefinition{..} -> do
        newLetDefinitionExpression <- desugarPatterns letDefinitionExpression
        pure $
          LetDefinition
            { letDefinitionExpression = newLetDefinitionExpression
            , ..
            }
