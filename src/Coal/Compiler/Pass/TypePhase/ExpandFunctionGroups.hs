{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TypePhase.ExpandFunctionGroups (FunctionGroupsTransform (..), passExpandFunctionGroups) where

import Coal.AST.Metadata (Metadata (..))
import Coal.AST.Shorthand (matchE, tupleE, tupleP, varE, varP)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language (Choice (..), Clause (..), Expression (..), Kind (..))
import Coal.Language.Definition
import Coal.Language.Module (Module (..))
import Coal.Language.Pattern
import Coal.Language.Trait (Qualified (..))
import Control.Monad.Trans (lift)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Extras (Name)
import TextShow (showt)

passExpandFunctionGroups :: (Monad m) => Pass Metadata m (Module Metadata Kind ()) (Module Metadata Kind ())
passExpandFunctionGroups = Pass{runPass = pass}

pass :: (Monad m) => Module Metadata Kind () -> CompilerT Metadata m (Module Metadata Kind ())
pass modul = do
  -- withCurrentModuleC expandFunctionGroups
  setCurrentModuleC modul
  expandFunctionGroups modul

class FunctionGroupsTransform e where
  expandFunctionGroups :: (Monad m) => e -> CompilerT Metadata m e

instance (FunctionGroupsTransform e) => FunctionGroupsTransform [e] where
  expandFunctionGroups = traverse expandFunctionGroups

instance (FunctionGroupsTransform e) => FunctionGroupsTransform (NonEmpty e) where
  expandFunctionGroups = traverse expandFunctionGroups

instance FunctionGroupsTransform (Module Metadata Kind ()) where
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
      pure [o]

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

--    DFunction loc name fs@(FunctionDefinition _ w _ ps _ :| _) gs ->
--      pure [DConstant loc name e1 gs]
--     where
--      e1 = ConstantDefinition loc w (Qualified [] ()) (toExpr (length ps) loc (NonEmpty.toList fs))
--
-- toExpr :: Int -> Metadata -> [FunctionDefinition Metadata ()] -> Expression Metadata () ()
-- toExpr n loc fs = ELambda loc (varP <$> args) (matchE (var args) clauses)
-- where
--  ns = NonEmpty.fromList [1 .. n]
--  args = (<>) "$arg_" . showt <$> ns
--  clauses =
--    case [EClause a (pat ps) (CPlain mempty [] e :| []) | FunctionDefinition a _ _ ps e <- fs] of
--      c : cs ->
--        c :| cs
--      [] ->
--        error "Implementation error"
--  pat ps
--    | length ps == 1 = NonEmpty.head ps
--    | otherwise = tupleP ps
--  var qs
--    | length qs == 1 = varE (NonEmpty.head qs)
--    | otherwise = tupleE (varE <$> qs)
