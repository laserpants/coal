{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TranslationPhase.ExpandPatterns (passExpandPatterns) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Journal
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language (IndexedType, Kind (..))
import Coal.Language.Definition
import Coal.Language.Expression (Clause (..), Expression (..))
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Expression.Choice (Choice (..))
import Coal.Language.HasType (HasType (..), foldTypeOf)
import Coal.Language.Module
import Coal.Language.Module.Path
import Coal.Language.Pattern (Pattern (..))
import Control.Monad.Trans (lift)
import Data.Generics.Uniplate.Data (descendM)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Extras (Name)

passExpandPatterns :: (Monad m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passExpandPatterns = Pass{runPass = bork}

bork :: (Monad m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
bork = desugarPatterns

class TransformContext s where
  desugarPatterns :: (Monad m) => s -> CompilerT Metadata m s

instance TransformContext (Pattern Metadata Kind IndexedType) where
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

-- instance TransformContext (IndexedPattern Metadata) where
--  desugarPatterns =
--    \case
--      p@PVariable{} ->
--        pure p
--      p@(PAnnotation _ _ PVariable{}) ->
--        pure p
--      PShorthand loc (Label t name) ->
--        desugarPatterns (PVariable loc (Label t name))
--      p -> do
--        name <- lift $ supplied (freshName "v")
--        tellPatterns1 (name, p)
--        pure (PVariable mempty (Label (typeOf p) name))

instance TransformContext (Binding Expression Metadata Kind IndexedType) where
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

instance TransformContext (Expression Metadata Kind IndexedType) where
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

-- instance TransformContext (FunctionDefinition Metadata IndexedType) where
--  desugarPatterns =
--    \case
--      FunctionDefinition a u w ps e -> do
--        error "!1"
--
----        e1 <- desugarPatterns e
----        (qs, rs) <- listenPatterns (traverse desugarPatterns ps)
----        pure (FunctionDefinition a u w qs (foldr (unrollMatch a) e1 rs))
--
-- instance TransformContext (ConstantDefinition Metadata IndexedType) where
--  desugarPatterns =
--    \case
--      ConstantDefinition a u w e ->
--        error "!2"
--
----        ConstantDefinition a u w <$> desugarPatterns e
--
-- instance TransformContext (Definition Metadata Kind IndexedType) where
--  desugarPatterns =
--    \case
--      DFunction loc name f fs ->
--        DFunction loc name <$> traverse desugarPatterns f <*> traverse desugarPatterns fs
--      DConstant loc name g fs ->
--        DConstant loc name <$> desugarPatterns g <*> traverse desugarPatterns fs
--      DInstance loc n (InstanceDefinition ts pt ds) ->
--        DInstance loc n . InstanceDefinition ts pt <$> traverse desugarPatterns ds
--      d ->
--        pure d
--
-- instance TransformContext (Module Metadata Kind IndexedType) where
--  desugarPatterns =
--    \case
--      Module p ns ds ->
--        Module p ns <$> traverse desugarPatterns ds

instance TransformContext (Module Metadata Kind IndexedType) where
  desugarPatterns =
    \case
      Module{..} -> do
        newModuleDefinitions <- traverse desugarPatterns protoOmoduleDefinitions
        pure
          Module
            { protoOmoduleDefinitions = newModuleDefinitions
            , ..
            }

instance TransformContext (Definition Metadata Kind IndexedType) where
  desugarPatterns =
    \case
      DFunction loc name def ->
        DFunction loc name <$> desugarPatterns def
      DLet loc name def ->
        DLet loc name <$> desugarPatterns def
      DInstance loc InstanceDefinition{..} -> do
        newInstanceDefinitionImplementations <- traverse desugarPatterns protoOinstanceDefinitionImplementations
        pure $
          DInstance
            loc
            InstanceDefinition
              { protoOinstanceDefinitionImplementations = newInstanceDefinitionImplementations
              , ..
              }
      d ->
        pure d

instance TransformContext (FunctionDefinition Metadata Kind IndexedType) where
  desugarPatterns =
    \case
      FunctionDefinition{..} -> do
        newFunctionDefinitionExpression <- desugarPatterns protoOfunctionDefinitionExpression
        (qs, rs) <- listenPatterns (traverse desugarPatterns protoOfunctionDefinitionPatterns)
        pure $
          FunctionDefinition
            { protoOfunctionDefinitionPatterns =
                qs
            , protoOfunctionDefinitionExpression =
                foldr (unrollMatch protoOfunctionDefinitionMetadata) newFunctionDefinitionExpression rs
            , ..
            }

instance TransformContext (LetDefinition Metadata Kind IndexedType) where
  desugarPatterns =
    \case
      LetDefinition{..} -> do
        newLetDefinitionExpression <- desugarPatterns protoOletDefinitionExpression
        pure $
          LetDefinition
            { protoOletDefinitionExpression = newLetDefinitionExpression
            , ..
            }
