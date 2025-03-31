{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Eval (Eval (..), Value (..), eval) where

import Control.Monad.Reader (MonadReader, Reader, ask, local, runReader)
import Data.Char (isUpper)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Noll.Common.Environment (Environment (..))
import Noll.Common.List1 (fromList1)
import Lang.Label (Label (..), labelName)
import Noll.Language.Expression (CompiledClause (..), Expression (..))
import Noll.Language.Expression.Operator.Binary (BinaryOperator (..))
import Noll.Language.Primitive (Primitive (..))
import Noll.Utils (Name, forM)
import TextShow (showt)

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Noll.Common.Environment as Environment

data Value
  = VData Name [Value]
  | VPrim Primitive
  | VFail
  | VFun [Name] (Eval Value)

instance Eq Value where
  VData c1 vs1 == VData c2 vs2 =
    c1 == c2 && vs1 == vs2
  VPrim p1 == VPrim p2 =
    p1 == p2
  VFail == VFail =
    True
  VFail == _ =
    False
  _ == VFail =
    False
  _ == _ =
    error "Not comparable"

instance Show Value where
  showsPrec d =
    \case
      VPrim prim ->
        showParen
          (d > 10)
          (showString "VPrim " . showsPrec 11 prim)
      VData name vals ->
        showParen
          (d > 10)
          (showString ("VData " <> show name <> " ") . showsPrec 11 vals)
      VFail{} ->
        showString "VFail"
      VFun{} ->
        showString "<<function>>"

newtype Eval e = Eval {evalMonad :: Reader (Environment Value) e}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (Environment Value)
    )

{-# INLINE runEval #-}
runEval :: Environment Value -> Eval e -> e
runEval env a = runReader (evalMonad a) env

evalVar :: Name -> Eval Value
evalVar name = do
  env <- ask
  case Map.lookup name (envDictionary env) of
    Nothing ->
      error ("Not in scope: " <> show name)
    Just v ->
      pure v

evalExpr :: (Show a, Show t) => Expression a t -> Eval Value
evalExpr =
  \case
    EAnnotation _ _ e ->
      evalExpr e
    ELiteral _ p ->
      pure (VPrim p)
    EVariable _ (Label _ name) ->
      evalVar name
    EConstructor _ (Label _ name) ->
      pure (VData name [])
    ECompiledMatch _ _ e cs -> do
      v1 <- evalExpr e
      vs <- forM cs $
        \case
          ECompiledClause ls e ->
            matchClause (fromList1 (labelName <$> ls)) [v1] e
          ECompiledField{} ->
            error "TODO"
      pure $ fromMaybe VFail (find (/= VFail) vs)
    EIf _ _ e1 e2 e3 -> do
      v1 <- evalExpr e1
      case v1 of
        VPrim (LBool True) ->
          evalExpr e2
        VPrim (LBool False) ->
          evalExpr e3
        _ ->
          error "Non-boolean if-condition"
    EApplication _ _ e1 es -> do
      v1 <- evalExpr e1
      vs <- traverse evalExpr es
      case v1 of
        VFun names f
          | arity == length vs ->
              local (Environment.insertMultiple (names `zip` fromList1 vs)) f
         where
          arity = length names
        VData con vs1 ->
          pure (VData con (vs1 <> fromList1 vs))
    EBinaryOperator _ _ OEqualTo -> do
      pure $ args2 $ \a0 a1 ->
        case (a0, a1) of
          (VPrim (LInt32 a), VPrim (LInt32 b)) ->
            pure (VPrim (LBool (a == b)))
    ELambda{} ->
      error "TODO"
    ELet{} ->
      error "TODO"
    ERecursiveLet{} ->
      error "TODO"
    EUnaryOperator{} ->
      error "TODO"
    EBinaryOperator{} ->
      error "TODO"
    ERecord{} ->
      error "TODO"
    EListCons{} ->
      error "TODO"
    EMatch{} ->
      error "TODO"
    ESelect{} ->
      error "TODO"
    EFold{} ->
      error "TODO"
    EListLiteral{} ->
      error "TODO"

{-# INLINE argn #-}
argn :: Int -> Name
argn n = "$$$." <> showt n

-- args1 :: (Value -> Eval Value) -> Value
-- args1 f = VFun [argn 0] $ do
--  a0 <- evalVar (argn 0)
--  f a0

args2 :: (Value -> Value -> Eval Value) -> Value
args2 f = VFun [argn 0, argn 1] $ do
  a0 <- evalVar (argn 0)
  a1 <- evalVar (argn 1)
  f a0 a1

matchClause :: (Show a, Show t) => [Name] -> [Value] -> Expression a t -> Eval Value
matchClause (name : names) (VData name1 vs1 : vs2) e
  | isUpper (Text.head name) =
      if name == name1
        then matchClause names (vs1 <> vs2) e
        else pure VFail
matchClause (name : names) (v : vs) e =
  local (Environment.insert name v) (matchClause names vs e)
matchClause [] [] e =
  evalExpr e
matchClause _ _ _ =
  error "error: matchClause"

{-# INLINE eval #-}
eval :: (Show a, Show t) => Environment Value -> Expression a t -> Value
eval env = runEval env . evalExpr
