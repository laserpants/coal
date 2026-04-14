{-# LANGUAGE FlexibleContexts #-}

module Coal.Compiler.Pass.PreflightPhase.DesugarWhereClauses (
  passDesugarWhereClauses,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Pass (Pass (..), mapPass)
import Coal.Compiler.Stack
import Coal.Language.Module (Module (..))
import Control.Monad.IO.Class (MonadIO)
import Data.Data (Data)

passDesugarWhereClauses :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata k t)] [BuildEnvelope (Module Metadata k t)]
passDesugarWhereClauses = mapPass $ Pass{runPass = traverse passImpl}

-- TODO:
passImpl :: (MonadIO m) => Module Metadata k t -> CompilerT Metadata m (Module Metadata k t)
passImpl = return

-- liftWhereClause :: (Monad m) => Name -> Definition Metadata k t -> CompilerT Metadata m (Definition Metadata k t)
-- liftWhereClause name =
--  \case
--    DFunction loc old f _ -> do
--      new <- manufacturedName name old
--      pure (DFunction loc new f [])
--    DConstant loc old c _ -> do
--      new <- manufacturedName name old
--      pure (DConstant loc new c [])
--    d ->
--      pure d
--
-- manufacturedName :: (Monad m) => Name -> Name -> CompilerT Metadata m Name
-- manufacturedName name old = do
--  tellDesugarWhereClauses [(old, new)]
--  pure new
-- where
--  new = name <> "__$local_" <> old
--
-- expandDesugarWhereClausesModule :: (Monad m, Ord t, Data t) => Module Metadata k t -> CompilerT Metadata m (Module Metadata k t)
-- expandDesugarWhereClausesModule (Module p ns ds) =
--  Module p ns . concat <$> traverse expandDesugarWhereClausesDefinition ds
--
-- expandDesugarWhereClausesDefinition :: (Monad m, Ord t, Data t) => Definition Metadata k t -> CompilerT Metadata m [Definition Metadata k t]
-- expandDesugarWhereClausesDefinition =
--  \case
--    DFunction loc name f ws -> do
--      (ds, names) <- listenDesugarWhereClauses $ traverse (liftWhereClause name) ws
--      pure (replaceNames names <$> (ds <> [DFunction loc name f []]))
--    DConstant loc name c ws -> do
--      (ds, names) <- listenDesugarWhereClauses $ traverse (liftWhereClause name) ws
--      pure (replaceNames names <$> (ds <> [DConstant loc name c []]))
--    d ->
--      pure [d]
--
-- replaceNames :: (Data t, Ord t) => [(Name, Name)] -> Definition Metadata k t -> Definition Metadata k t
-- replaceNames names =
--  \case
--    DFunction loc n f _ ->
--      DFunction loc n (fmap (replaceFunctionNames names) f) []
--    DConstant loc n c _ ->
--      DConstant loc n (replaceConstantNames names c) []
--    d ->
--      d
--
-- replaceFunctionNames :: (Data t, Ord t) => [(Name, Name)] -> FunctionDefinition Metadata t -> FunctionDefinition Metadata t
-- replaceFunctionNames names =
--  \case
--    FunctionDefinition a u w ps e ->
--      FunctionDefinition a u w ps (foldr (uncurry rename) e names)
--
-- replaceConstantNames :: (Data t, Ord t) => [(Name, Name)] -> ConstantDefinition Metadata t -> ConstantDefinition Metadata t
-- replaceConstantNames names =
--  \case
--    ConstantDefinition a u w e ->
--      ConstantDefinition a u w (foldr (uncurry rename) e names)
