{-# LANGUAGE LambdaCase #-}

{- |
Type checking environment.

Maintains the symbol tables and context stack for type checking:

  * Global variables (top-level functions and constants)
  * Data constructors
  * Local variables (lambda parameters, let-bindings, case patterns)
  * Current context (for error reporting)

The environment is threaded through the checker using 'ReaderT', with local
bindings introduced via 'withLocals'.
-}
module Coal.Kernel.TypeCheck.Env (
  CheckEnv (..),
  buildGlobalEnv,
  lookupName,
  lookupCon,
  withLocals,
  setContext,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Language.Type.HasType (HasType (..), foldType)
import Coal.Kernel.TypeCheck.Error (Context (..))
import Common (Name)

data CheckEnv = CheckEnv
  { globalVars :: Map Name Type
  , constructors :: Map Name Type
  , locals :: Map Name Type
  , currentCtx :: Context
  }

{- | Build a global environment from a list of modules.

All module objects are collected into a single symbol table.
-}
buildGlobalEnv :: [Module Type] -> CheckEnv
buildGlobalEnv modules =
  CheckEnv
    { globalVars = foldMap moduleVars modules
    , constructors = foldMap moduleCons modules
    , locals = Map.empty
    , currentCtx = InExpression
    }

moduleVars :: Module Type -> Map Name Type
moduleVars m = foldMap objectVar (moduleObjects m)

objectVar :: Object Type -> Map Name Type
objectVar = \case
  DFunction _ name params body ->
    Map.singleton name (foldType (typeOf body) (typeOf <$> params))
  DConstant name expr ->
    Map.singleton name (typeOf expr)
  DExternal name t ->
    Map.singleton name t
  DData{} ->
    Map.empty

moduleCons :: Module Type -> Map Name Type
moduleCons m = foldMap objectCon (moduleObjects m)

objectCon :: Object Type -> Map Name Type
objectCon = \case
  DData _ ctors -> Map.fromList ctors
  _ -> Map.empty

-- | Look up a name: locals shadow globals.
lookupName :: Name -> CheckEnv -> Maybe Type
lookupName name env =
  case Map.lookup name (locals env) of
    Just t -> Just t
    Nothing -> Map.lookup name (globalVars env)

-- | Look up a constructor by name.
lookupCon :: Name -> CheckEnv -> Maybe Type
lookupCon name env = Map.lookup name (constructors env)

-- | Extend the local scope with additional bindings.
withLocals :: [(Name, Type)] -> CheckEnv -> CheckEnv
withLocals bs env =
  env{locals = Map.fromList bs `Map.union` locals env}

-- | Update the context for error reporting.
setContext :: Context -> CheckEnv -> CheckEnv
setContext ctx env = env{currentCtx = ctx}
