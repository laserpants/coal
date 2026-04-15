{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TypePhase.ExpandFunctionGroups (
  ExpandContext (..),
  passExpandFunctionGroups,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.AST.Shorthand (matchE, tupleE, tupleP, varE, varP)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT, setCurrentModuleC)
import Coal.Language (Choice (..), Clause (..), Expression (..), Kind (..))
import Coal.Language.Definition
import Coal.Language.Module (Module (..))
import Coal.Language.Pattern (Pattern)
import Coal.Language.Trait (Qualified (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Extras (Name)
import TextShow (showt)

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
                  ELambda loc (varP <$> args) (matchE (var args) (clauses defs))
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

clauses :: (Monoid a) => [FunctionDefinition a k ()] -> NonEmpty (Clause a k ())
clauses defs =
  case [ EClause a (pat ps) (CPlain mempty [] e :| [])
       | FunctionDefinition a _ _ ps e <- defs
       ] of
    c : cs ->
      c :| cs
    [] ->
      error "Implementation error"

pat :: (Monoid a) => NonEmpty (Pattern a k ()) -> Pattern a k ()
pat ps
  | length ps == 1 =
      NonEmpty.head ps
  | otherwise =
      tupleP ps

var :: (Monoid a) => NonEmpty Name -> Expression a k ()
var qs
  | length qs == 1 =
      varE (NonEmpty.head qs)
  | otherwise =
      tupleE (varE <$> qs)
