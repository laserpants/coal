{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TranslationPhase.ExpandGuards (passExpandGuards) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT)
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule
import Control.Monad.Trans (lift)
import Data.Foldable (foldrM)
import Data.Generics.Uniplate.Data (transformBiM)
import Data.List.NonEmpty (NonEmpty (..), tails)
import qualified Data.List.NonEmpty as NonEmpty
import Extras (Map, traverseM)

passExpandGuards :: (Monad m) => Pass Metadata m (ProtoModule Metadata Kind IndexedType) (ProtoModule Metadata Kind IndexedType)
passExpandGuards = Pass{runPass = pass}

pass :: (Monad m) => ProtoModule Metadata Kind IndexedType -> CompilerT Metadata (ProtoCompilerT m Metadata) (ProtoModule Metadata Kind IndexedType)
pass = bork

trivial :: Clause a Kind t -> Bool
trivial (EClause _ _ (CPlain _ [] _ :| [])) = True
trivial _ = False

expandExpression :: (Monad m) => Expression Metadata Kind IndexedType -> CompilerT Metadata (ProtoCompilerT m Metadata) (Expression Metadata Kind IndexedType)
expandExpression =
  \case
    e@(EMatch _ _ _ cs)
      | all trivial cs ->
          pure e
    EMatch a t e cs -> do
      e' <- expandExpression e
      name <- lift $ supplied (freshName "scr")
      let ll = Label (typeOf e) name
          var = EVariable a ll
      cs' <- traverse (expandClauseGuards a t var) (NonEmpty.init $ tails cs)
      case cs' of
        x : xs ->
          pure $
            ELet
              a
              (BPattern a (PVariable a ll) e' :| [])
              (EMatch a t var (x :| xs))
        [] ->
          error "Implementation error"
    e ->
      pure e

class ExpandGuards a where
  expandGuards :: (Monad m) => a -> CompilerT Metadata m (NonEmpty a)

instance (ExpandGuards a) => ExpandGuards [a] where
  expandGuards = traverseM expandGuards

instance (ExpandGuards a) => ExpandGuards (NonEmpty a) where
  expandGuards = traverseM expandGuards

instance (ExpandGuards a) => ExpandGuards (Map k a) where
  expandGuards = traverseM expandGuards

instance (ExpandGuards a) => ExpandGuards (Maybe a) where
  expandGuards = traverseM expandGuards

bork :: (Monad m) => ProtoModule Metadata Kind IndexedType -> CompilerT Metadata (ProtoCompilerT m Metadata) (ProtoModule Metadata Kind IndexedType)
bork =
  \case
    ProtoModule{..} -> do
      newModuleDefinitions <- traverse fnork protoOmoduleDefinitions
      return $
        ProtoModule
          { protoOmoduleDefinitions = newModuleDefinitions
          , ..
          }

fnork :: (Monad m) => ProtoDefinition Metadata Kind IndexedType -> CompilerT Metadata (ProtoCompilerT m Metadata) (ProtoDefinition Metadata Kind IndexedType)
fnork =
  \case
    ProtoDFunction loc name def -> do
      ProtoDFunction loc name <$> transformBiM expandExpression def
    ProtoDLet loc name def ->
      ProtoDLet loc name <$> transformBiM expandExpression def
    ProtoDInstance loc ProtoInstanceDefinition{..} -> do
      newInstanceDefinitionImplementations <- traverse fnork protoOinstanceDefinitionImplementations
      return $
        ProtoDInstance
          loc
          ProtoInstanceDefinition
            { protoOinstanceDefinitionImplementations = newInstanceDefinitionImplementations
            , ..
            }
    d ->
      return d

expandClauseGuards :: (Monad m) => Metadata -> IndexedType -> Expression Metadata Kind IndexedType -> [Clause Metadata Kind IndexedType] -> CompilerT Metadata (ProtoCompilerT m Metadata) (Clause Metadata Kind IndexedType)
expandClauseGuards _ _ _ (c@(EClause _ _ (CPlain _ [] _ :| [])) : _) =
  pure c
expandClauseGuards a1 t var (EClause a2 p choices : clauses) = do
  next <- expandExpression (EMatch a1 t var (NonEmpty.fromList $ clauses <> [EClause a3 (PAny a3 (typeOf q)) cs2]))
  e1 <- foldrM go next choices
  pure $ EClause a2 p (CPlain a1 [] e1 :| [])
 where
  EClause a3 q cs2 = last clauses
  go :: (Monad m) => Choice Expression Metadata Kind IndexedType -> Expression Metadata Kind IndexedType -> CompilerT Metadata m (Expression Metadata Kind IndexedType)
  go (CPlain a gs e) e1 =
    pure $ EIf a (typeOf e1) (foldr1 (conjunction a) (guardExpression <$> gs)) e e1
expandClauseGuards _ _ _ _ =
  error "Implementation error"

conjunction :: Metadata -> Expression Metadata Kind IndexedType -> Expression Metadata Kind IndexedType -> Expression Metadata Kind IndexedType
conjunction a e1 e2 = EApplication a (TIntrinsic IBool) e1 (e2 :| [])
