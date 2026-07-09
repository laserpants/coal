{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Evaluation monad and error handling.

Provides the 'EvalM' monad for interpreter execution:

  * 'ExceptT' for structured error handling
  * 'ReaderT' for the evaluation environment
  * 'IO' for external effects

Defines the 'EvalEnv' type for variable bindings and external function tables,
and the 'EvalError' type for runtime failures.
-}
module Coal.Kernel.Eval.State (
  EvalError (..),
  EvalEnv (..),
  EvalM (..),
  runEvalM,
  throwEval,
  lookupVar,
  extendEnv,
  extendEnvMany,
  getExterns,
  askEnv,
  liftIO,
) where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.IO.Class as IO
import Control.Monad.Reader (MonadReader, ReaderT, ask, local, runReaderT)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Coal.Common.Name (Name)
import Coal.Kernel.Eval.Value (Value (..))

-- ---------------------------------------------------------------------------
-- Error model
-- ---------------------------------------------------------------------------

data EvalError
  = -- | A variable or function name was not found in any scope.
    UnboundName Name
  | -- | An external function name was called but has no registered handler.
    UnboundExternal Name
  | -- | A function was given the wrong number of arguments.
    ArityMismatch Name Int Int
  | -- | No case clause matched the scrutinee.
    PatternMatchFailure String
  | -- | A runtime type invariant was violated (e.g. expected VBool, got VInt32).
    TypeMismatch String String
  | -- | A language construct not yet supported by this evaluator.
    Unsupported String
  | -- | An error raised by a host external handler.
    ExternalError Name String
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Evaluation environment
-- ---------------------------------------------------------------------------

data EvalEnv = EvalEnv
  { envBindings :: Map Name Value
  -- ^ Local and global variable/function bindings.
  , envExterns :: Map Name ([Value] -> IO (Either EvalError Value))
  -- ^ Host-provided handlers for external functions (coal_* etc.).
  -- Using IO so handlers can perform real side effects.
  }

-- ---------------------------------------------------------------------------
-- Monad
-- ---------------------------------------------------------------------------

newtype EvalM a = EvalM
  { unEvalM :: ExceptT EvalError (ReaderT EvalEnv IO) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader EvalEnv
    , MonadError EvalError
    , MonadIO
    )

instance MonadFail EvalM where
  fail = throwEval . Unsupported

runEvalM :: EvalEnv -> EvalM a -> IO (Either EvalError a)
runEvalM env (EvalM m) = runReaderT (runExceptT m) env

throwEval :: EvalError -> EvalM a
throwEval = throwError

-- | Lift an IO action into EvalM.
liftIO :: IO a -> EvalM a
liftIO = IO.liftIO

-- ---------------------------------------------------------------------------
-- Environment helpers
-- ---------------------------------------------------------------------------

askEnv :: EvalM EvalEnv
askEnv = ask

lookupVar :: Name -> EvalM Value
lookupVar name = do
  env <- askEnv
  case Map.lookup name (envBindings env) of
    Just v -> return v
    Nothing ->
      case Map.lookup name (envExterns env) of
        Just _ -> return (VExtern name)
        Nothing -> throwEval (UnboundName name)

extendEnv :: Name -> Value -> EvalM a -> EvalM a
extendEnv name val = local (\e -> e{envBindings = Map.insert name val (envBindings e)})

extendEnvMany :: [(Name, Value)] -> EvalM a -> EvalM a
extendEnvMany pairs = local (\e -> e{envBindings = Map.fromList pairs `Map.union` envBindings e})

getExterns :: EvalM (Map Name ([Value] -> IO (Either EvalError Value)))
getExterns = fmap envExterns ask
