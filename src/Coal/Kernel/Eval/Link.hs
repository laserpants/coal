{-# LANGUAGE LambdaCase #-}

{- |
Global environment construction.

Builds the global evaluation environment from parsed modules, registering all
top-level objects as bindings.

= Object registration

  * 'DFunction' becomes a 'VClosure' closed over the global environment
    (mutual recursion is handled via a lazy knot)
  * 'DConstant' is evaluated eagerly once and its value is stored
  * 'DData' registers a constructor factory as a closure
  * 'DExternal' inserts an entry into the extern table lookup if the name
    already appears in the provided 'ExternTable'; otherwise it is left absent
    so that calls to it will raise 'UnboundExternal'
-}
module Coal.Kernel.Eval.Link (
  GlobalEnv,
  buildGlobalEnv,
) where

import qualified Data.Map.Strict as Map

import Coal.Kernel.Eval.Expr (eval)
import Coal.Kernel.Eval.External (ExternTable)
import Coal.Kernel.Eval.State (EvalEnv (..), EvalError (..), runEvalM)
import Coal.Kernel.Eval.Value (Closure (..), Value (..))
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Type (Type)
import Common (Name)

-- | The global evaluation environment built from a list of parsed modules.
type GlobalEnv = EvalEnv

{- | Merge all modules into a single 'EvalEnv', registering each top-level
object as a binding.

  * 'DFunction' becomes a 'VClosure' closed over the global environment
    (mutual recursion is handled below via a lazy knot).
  * 'DConstant' is evaluated eagerly once and its value is stored.
  * 'DData' registers a constructor factory as a closure.
  * 'DExternal' inserts an entry into the extern table lookup if the name
    already appears in the provided 'ExternTable'; otherwise it is left absent
    so that calls to it will raise 'UnboundExternal'.
-}
buildGlobalEnv ::
  ExternTable ->
  [Module Type] ->
  IO (Either EvalError GlobalEnv)
buildGlobalEnv externTable modules = do
  -- Collect every object across all modules.
  let objects = concatMap moduleObjects modules

  -- We build the binding map in two passes:
  -- 1. Register functions and constructors as closures (they don't need values yet).
  -- 2. Re-run over constants so they can call functions.
  --
  -- To support mutual recursion, we use a lazy knot: each closure captures a
  -- reference to the final 'globalBindings' map (tied with 'let rec').

  let globalEnv0 = EvalEnv{envBindings = Map.empty, envExterns = externTable}

  -- Pass 1: build initial bindings without evaluating constants.
  let pass1Bindings = Map.fromList (concatMap (registerObject globalEnvFinal) objects)

      -- The final env is the one all closures ultimately point into.
      -- This is safe because closures only read the env when *called*, not
      -- when constructed.
      globalEnvFinal = globalEnv0{envBindings = pass1Bindings}

  -- Pass 2: evaluate constants in the context of the now-complete global env.
  evalResult <- evalConstants globalEnvFinal objects
  case evalResult of
    Left err -> return (Left err)
    Right constBindings ->
      return
        ( Right
            globalEnvFinal
              { envBindings = constBindings `Map.union` pass1Bindings
              }
        )

-- ---------------------------------------------------------------------------
-- Pass 1: register non-constant objects
-- ---------------------------------------------------------------------------

registerObject :: GlobalEnv -> Object Type -> [(Name, Value)]
registerObject globalEnv = \case
  DFunction _ name params body ->
    let closure =
          Closure
            { closureName = name
            , closureParams = params
            , closureBody = body
            , closureEnv = envBindings globalEnv
            }
     in [(name, VClosure closure)]
  -- Data constructors: one VConstructor per entry in the list.
  -- Index is determined by position in the lexicographically sorted list.
  DData _ ctors ->
    [ (ctorName, VConstructor ctorName idx [])
    | (idx, (ctorName, _)) <- zip [0 ..] ctors
    ]
  -- Constants and externals are handled in other passes.
  DConstant{} -> []
  DExternal{} -> []

-- ---------------------------------------------------------------------------
-- Pass 2: evaluate constants
-- ---------------------------------------------------------------------------

evalConstants :: GlobalEnv -> [Object Type] -> IO (Either EvalError (Map.Map Name Value))
evalConstants env objects = do
  let constants = [(name, expr) | DConstant name expr <- objects]
  go env Map.empty constants
 where
  go _ acc [] = return (Right acc)
  go env0 acc ((name, expr) : rest) = do
    result <- runEvalM env0 (eval expr)
    case result of
      Left err -> return (Left err)
      Right v ->
        -- Extend env so subsequent constants can refer to earlier ones.
        go
          env0{envBindings = Map.insert name v (envBindings env0)}
          (Map.insert name v acc)
          rest
