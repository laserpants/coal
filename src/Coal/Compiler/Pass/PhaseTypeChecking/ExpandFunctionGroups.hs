{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Pass.PhaseTypeChecking.ExpandFunctionGroups

Expand function groups into individual let definitions with pattern matching.

Function groups represent multiple equations defining a single function with
different patterns. This pass converts them into a single lambda expression
that performs explicit pattern matching on the arguments.

For example, a function group like:

@
fun f
  | 0 = "zero"
  | n = "non-zero"
@

is transformed into:

@
let f =
  fn($arg_1) =>
    match ($arg_1) {
      | 0 => "zero"
      | n => "non-zero"
    }
@

This normalization simplifies later type checking and compilation phases by
ensuring all functions have a uniform representation.
-}
module Coal.Compiler.Pass.PhaseTypeChecking.ExpandFunctionGroups (
  ExpandContext (..),
  passExpandFunctionGroups,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.AST.Shorthand (matchE, tupleE, tupleP, varE, varP)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT, setCurrentModuleC)
import Coal.Language (Choice (..), Clause (..), Expression (..), Kind (..), Pattern, Qualified (..))
import Coal.Language.Definition
import Coal.Language.Module (Module (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Extras (Name)
import TextShow (showt)

{- | Function group expansion pass.

Transform function groups (multiple equations for a single function) into
individual let definitions containing lambda expressions with explicit pattern
matching. This normalization ensures all functions have a uniform representation,
simplifying subsequent type inference and compilation.
-}
passExpandFunctionGroups :: (Monad m) => Pass Metadata m (Module Metadata Kind ()) (Module Metadata Kind ())
passExpandFunctionGroups = Pass{runPass = passImpl}

passImpl :: (Monad m) => Module Metadata Kind () -> CompilerT Metadata m (Module Metadata Kind ())
passImpl m = do
  setCurrentModuleC m
  expandFunctionGroups m

class ExpandContext e where
  expandFunctionGroups :: (Monad m) => e -> CompilerT Metadata m e

instance (ExpandContext e) => ExpandContext [e] where
  expandFunctionGroups = traverse expandFunctionGroups

instance (ExpandContext e) => ExpandContext (NonEmpty e) where
  expandFunctionGroups = traverse expandFunctionGroups

instance ExpandContext (Module Metadata Kind ()) where
  expandFunctionGroups =
    \case
      Module{..} -> do
        newDefinitions <- traverse expandGroups moduleDefinitions
        return $
          Module
            { moduleDefinitions = concat newDefinitions
            , ..
            }

-- TODO: annotations
expandGroups :: (Monad m) => Definition Metadata Kind () -> CompilerT Metadata m [Definition Metadata Kind ()]
expandGroups =
  \case
    DFunctionGroup loc name defs@(firstDef : _) ->
      return
        [ DLet
            loc
            name
            LetDefinition
              { letDefinitionMetadata = loc
              , letDefinitionAnnotation = Nothing
              , letDefinitionType = With [] ()
              , letDefinitionExpression =
                  ELambda loc (varP <$> args) (matchE (packVariables args) (buildExpressionClauses defs))
              }
        ]
     where
      FunctionDefinition{..} = firstDef
      ns = NonEmpty.fromList [1 .. length functionDefinitionPatterns]
      args = (<>) "$arg_" . showt <$> ns
    DInstance loc InstanceDefinition{..} -> do
      newImplementations <- traverse expandGroups instanceDefinitionImplementations
      return
        [ DInstance
            loc
            InstanceDefinition
              { instanceDefinitionImplementations = concat newImplementations
              , ..
              }
        ]
    o ->
      return [o]

buildExpressionClauses :: (Monoid a) => [FunctionDefinition a k ()] -> NonEmpty (Clause a k ())
buildExpressionClauses defs =
  case [ EClause functionDefinitionMetadata (packPatterns functionDefinitionPatterns) (CPlain mempty [] functionDefinitionExpression :| [])
       | FunctionDefinition{..} <- defs
       ] of
    c : cs ->
      c :| cs
    [] ->
      error "Internal compiler error: buildExpressionClauses called with empty list of function definitions."

packPatterns :: (Monoid a) => NonEmpty (Pattern a k ()) -> Pattern a k ()
packPatterns ps
  | length ps == 1 =
      NonEmpty.head ps
  | otherwise =
      tupleP ps

packVariables :: (Monoid a) => NonEmpty Name -> Expression a k ()
packVariables qs
  | length qs == 1 =
      varE (NonEmpty.head qs)
  | otherwise =
      tupleE (varE <$> qs)
