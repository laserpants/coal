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
import Debug.Trace
import Noll.Common.Environment (Environment (..))
import Noll.Common.List1 (NonEmpty (..), fromList1)
import Noll.Label (Label (..), labelName)
import Noll.Language.Expression (CompiledClause (..), Expression (..))
import Noll.Language.Expression.Operator.Binary (BinaryOperator (..))
import Noll.Language.Primitive (Primitive (..))
import Noll.Utils (Name, forM)

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Noll.Common.Environment as Environment

data Value
  = VData Name [Value]
  | VLiteral Primitive
  | VFail
  | VFun [Name] (Eval Value)

--instance Show Value where
--  show (VData name vs) = show (name, vs)
--  show (VLiteral p) = show p
--  show (VFail) = "VFail"

instance Eq Value where
  VData c1 vs1 == VData c2 vs2 =
    c1 == c2 && vs1 == vs2
  VLiteral p1 == VLiteral p2 =
    p1 == p2
  VFail == VFail =
    True
  VFail == _ =
    False
  _ == VFail =
    False
  _ == _ =
    error "Not comparable"

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
  case Map.lookup name (environmentDictionary env) of
    Nothing ->
      error ("Not in scope: " <> show name)
    Just v ->
      pure v

evalExpr :: (Show a, Show t) => Expression a t -> Eval Value
evalExpr =
  \case
    ELiteral _ p ->
      pure (VLiteral p)
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
      pure $ fromMaybe VFail (find (/= VFail) vs)
    EIf _ _ e1 e2 e3 -> do
      v1 <- evalExpr e1
      case v1 of
        VLiteral (LBool True) ->
          evalExpr e2
        VLiteral (LBool False) ->
          evalExpr e3
        _ ->
          error "Non-boolean if-condition"
    EApplication _ _ e1 es -> do
      v1 <- evalExpr e1
      vs <- traverse evalExpr es
      case v1 of
        VFun names f
          | arity == length vs ->
              local (Environment.insertMany (names `zip` fromList1 vs)) f
         where
          arity = length names
        VData con vs1 ->
          pure (VData con (vs1 <> fromList1 vs))
    EBinaryOperator _ (_, OEqualTo) -> do
      pure $ args2 $ \a0 a1 ->
        case (a0, a1) of
          (VLiteral (LInt32 a), VLiteral (LInt32 b)) ->
            pure (VLiteral (LBool (a == b)))

{-# INLINE argn #-}
argn :: Int -> Name
argn n = Text.pack ("$$$." <> show n)

args1 :: (Value -> Eval Value) -> Value
args1 f = VFun [argn 0] $ do
  a0 <- evalVar (argn 0)
  f a0

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

{-# INLINE eval #-}
eval :: (Show a, Show t) => Environment Value -> Expression a t -> Value
eval env = runEval env . evalExpr
