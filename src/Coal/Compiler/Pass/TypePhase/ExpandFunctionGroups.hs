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
import Coal.Language.Pattern
import Coal.Language.Trait (Qualified (..))
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))
import Control.Monad.Trans (lift)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Extras (Name)
import TextShow (showt)

passExpandFunctionGroups :: (Monad m) => Pass Metadata m (ProtoModule Metadata Kind ()) (ProtoModule Metadata Kind ())
passExpandFunctionGroups = Pass{runPass = pass}

pass :: (Monad m) => ProtoModule Metadata Kind () -> CompilerT Metadata m (ProtoModule Metadata Kind ())
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

instance FunctionGroupsTransform (ProtoModule Metadata Kind ()) where
  expandFunctionGroups =
    \case
      ProtoModule{..} -> do
        newDefinitions <- traverse expandGroups protoOmoduleDefinitions
        return $
          ProtoModule
            { protoOmoduleDefinitions = concat newDefinitions
            , ..
            }

-- TODO: annotations
expandGroups :: (Monad m) => ProtoDefinition Metadata Kind () -> CompilerT Metadata m [ProtoDefinition Metadata Kind ()]
expandGroups =
  \case
    ProtoDFunctionGroup loc name defs@(firstDef : _) ->
      return
        [ ProtoDLet
            loc
            name
            ProtoLetDefinition
              { protoOletDefinitionMetadata = loc
              , protoOletDefinitionAnnotation = Nothing
              , protoOletDefinitionType = With [] ()
              , protoOletDefinitionExpression =
                  ELambda loc (varP <$> args) (matchE (var args) (clauses defs))
              }
        ]
     where
      ProtoFunctionDefinition{..} = firstDef
      ns = NonEmpty.fromList [1 .. length protoOfunctionDefinitionPatterns]
      args = (<>) "$arg_" . showt <$> ns
    ProtoDInstance loc ProtoInstanceDefinition{..} -> do
      newImplementations <- traverse expandGroups protoOinstanceDefinitionImplementations
      return
        [ ProtoDInstance
            loc
            ProtoInstanceDefinition
              { protoOinstanceDefinitionImplementations = concat newImplementations
              , ..
              }
        ]
    o ->
      pure [o]

clauses :: (Monoid a) => [ProtoFunctionDefinition a k ()] -> NonEmpty (Clause a k ())
clauses defs =
  case [ EClause a (pat ps) (CPlain mempty [] e :| [])
       | ProtoFunctionDefinition a _ _ ps e <- defs
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
