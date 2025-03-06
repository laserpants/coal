{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.Compiler where

import Control.Arrow ((>>>))
import Control.Monad.RWS (RWS, ask, evalRWS, local)
import Control.Monad.State (MonadState, State, evalState, gets, modify, runState, runStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (MonadWriter, Writer, runWriter, tell)
import Data.Fix (Fix (..))
import Data.Functor.Foldable (cata, embed, project)
import Data.List (partition)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Noll.AST.FreeVars (FreeVars (..), exceptNames)
import Noll.Common.Environment (Environment)
import qualified Noll.Common.Environment as Environment
import Noll.Common.List1 (List1, NonEmpty (..), fromList1)
import qualified Noll.Common.List1 as List1
import Noll.Common.Supply (supplied)
import Noll.Core.LLVM.IRConstruct (IRConstruct (..))
import Noll.Core.LLVM.IRInstruction.Interpreter (
  IRInterpreter (..),
  IRInterpreterArtifact (..),
  IRInterpreterEnv (..),
  IRInterpreterState (..),
  IRLine (..),
  inConstructorEnv,
  inValueEnv,
  runInterpreter,
 )
import Noll.Core.LLVM.IRInstruction.Interpreter.Object (
  objectEnvironment,
  objectInterpreter,
 )
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Core.Language (
  Binding (..),
  Clause (..),
  Expr,
  ExprF (..),
  Focus (..),
  Type,
  Typed (..),
  bindingLabel,
  foldType,
  overBindingLabel,
  unzipBindings,
 )
import qualified Noll.Core.Language as Core
import Noll.Core.Language.Expr.Replace (Sub, relabel)
import Noll.Core.Language.Object (Object (..), ObjectList, objectName)
import Noll.Core.Language.Type.Arrow (isFunction)
import Noll.Label (Label (..), labelName)
import Noll.Utils (
  Dictionary,
  Name,
  Over,
  applyM1,
  applyM2,
  foldrM,
  forM,
  isConstructor,
  traverse2,
  (<$$>),
 )
import Noll.Utils.Operators ((||.))
import TextShow

-------------------------------------------------------------------------------

runLifting :: RWS Name ObjectList Int a -> (a, ObjectList)
runLifting e = evalRWS e "" 1

functionType :: (Functor f, Foldable f, Typed t, Typed u) => t -> f u -> Type
functionType a as = foldType (typeOf a) (typeOf <$> as)

liftLambdas :: ObjectList -> ObjectList
liftLambdas objs = objs1 <> objs2
 where
  (objs1, objs2) =
    runLifting (traverse (traverse go) objs)
  go =
    cata $
      \case
        Core.ELet vs e -> do
          ws <- forM vs $ \(Binding ll@(Label _ name) e1) -> do
            f <- local (const name) e1
            pure (Binding ll f)
          f <- local mempty e
          pure (Core.let_ ws f)
        Core.ELam vs e -> do
          n <- supplied id
          name <- ask
          f <- local mempty e
          moveUp (if Text.null name then "$fn." <> showt n else name) vs f
        e ->
          local mempty (embed <$> sequence e)

moveUp :: (MonadWriter ObjectList m) => Name -> List1 (Label Type) -> Expr Type -> m (Expr Type)
moveUp name vs f = do
  tell [OFunction name (fromList1 vs) f]
  pure (Core.var (Label (functionType f vs) name))

-------------------------------------------------------------------------------

toObject :: Binding Type (Expr Type) -> Object Type (Expr Type)
toObject (Binding (Label _ name) e1) = go e1
 where
  go =
    project
      >>> \case
        Core.ELam vs e ->
          OFunction name (fromList1 vs) e
        e ->
          OConstant name (embed e)

isPrim :: Expr Type -> Bool
isPrim =
  cata $
    \case
      ELit{} ->
        True
      _ ->
        False

memoize :: (MonadWriter [Binding Type (Expr Type)] m) => Expr Type -> m (Expr Type)
memoize =
  project
    >>> \case
      Core.ELet vs e -> do
        let (ps, qs) = partition (not . (isFunction . bindingLabel ||. isPrim . bindingExpr)) (fromList1 vs)
        tell (Core.mem <$$> ps)
        case qs of
          u : us ->
            pure (Core.let_ (u :| us) e)
          [] ->
            pure e
      e ->
        pure (embed e)

transLetLifting :: (MonadWriter [Binding Type (Expr Type)] m) => Expr Type -> m (Expr Type)
transLetLifting =
  cata $
    \case
      Core.ELet vs e -> do
        as <- traverse sequence vs
        let (fs, es) = List1.partition (isFunction . bindingLabel) as
        tell fs
        case es of
          w : ws ->
            Core.let_ (w :| ws) <$> e
          [] ->
            e
      e ->
        embed <$> sequence e

-------------------------------------------------------------------------------

transSuffixExpr :: (MonadState Int m) => Expr t -> m (Expr t)
transSuffixExpr =
  cata $
    \case
      Core.ELet vs e -> do
        let (lls, es) = unzipBindings vs
        (lls1, a1, a2) <- applyM2 (addSuffix2 lls) (sequence es) e
        pure (Core.let_ (List1.zipWith Binding lls1 a1) a2)
      Core.ELam lls e -> do
        (lls1, a1) <- applyM1 (addSuffix lls) e
        pure (Core.lam lls1 a1)
      Core.ESel (Focus name ll2 ll3) e1 e2 -> do
        a1 <- e1
        (lls1, a2) <- applyM1 (addSuffix (ll2 :| [ll3])) e2
        case lls1 of
          (lls4 :| lls5 : _) ->
            pure (Core.sel (Focus name lls4 lls5) a1 a2)
          _ ->
            error "Implementation error"
      Core.EMat t e cs ->
        Core.match t
          <$> e
          <*> (traverse transSuffixClause =<< traverse sequence cs)
      e ->
        embed <$> sequence e

transSuffixClause :: (MonadState Int m) => Clause t (Expr t) -> m (Clause t (Expr t))
transSuffixClause =
  \case
    Clause lls e -> do
      (lls1, a) <- addSuffix lls e
      pure (Clause lls1 a)

addSuffix :: (MonadState Int m, Sub s) => List1 (Label t) -> s -> m (List1 (Label t), s)
addSuffix lls e = do
  (lls1, sub) <- mapping lls
  pure (lls1, relabel sub e)

addSuffix2 :: (MonadState Int m, Sub s1, Sub s2) => List1 (Label t) -> s1 -> s2 -> m (List1 (Label t), s1, s2)
addSuffix2 lls e1 e2 = do
  (lls1, sub) <- mapping lls
  pure (lls1, relabel sub e1, relabel sub e2)

mapping :: (MonadState Int m) => List1 (Label t) -> m (List1 (Label t), Dictionary Name)
mapping lls = runStateT (traverse go lls) mempty
 where
  go ll@(Label t name)
    | isConstructor name =
        pure ll
    | otherwise = do
        n <- lift (supplied id)
        let name1 = name <> ".[" <> showt n <> "]"
        modify (Map.insert name name1)
        pure (Label t name1)

-------------------------------------------------------------------------------

flattenELam :: Expr Type -> Expr Type
flattenELam =
  cata $
    \case
      Core.ELam vs1 (Fix (Core.ELam vs2 e1)) ->
        Core.lam (vs1 <> vs2) e1
      e ->
        embed e

-------------------------------------------------------------------------------

flattenEApp :: Expr t -> Expr t
flattenEApp =
  cata $
    \case
      Core.EApp t (Fix (Core.EApp _ e1 es1)) es2 ->
        Core.app t e1 (es1 <> es2)
      e ->
        embed e

-------------------------------------------------------------------------------

simplifyELet :: Expr t -> Expr t
simplifyELet e = relabel (Map.fromList sub) e1
 where
  subst =
    cata $
      \case
        ELet vs f -> do
          binds <- foldrM go [] =<< traverse sequence vs
          case binds of
            a : as ->
              Core.let_ (a :| as) <$> f
            [] ->
              f
        f ->
          embed <$> sequence f

  go (Binding ll1 (Fix (Core.EVar ll2))) ls = do
    tell [(labelName ll1, labelName ll2)]
    pure ls
  go l ls =
    pure (l : ls)
  (e1, sub) =
    runWriter (subst e)

-------------------------------------------------------------------------------

evalWS0 :: RWS () w Int a -> (a, w)
evalWS0 v = evalRWS v () 0

{-# INLINE notConstructor #-}
notConstructor :: Label t -> Bool
notConstructor = not . isConstructor . labelName

freeSet :: (Foldable f, FreeVars e t) => f Name -> e -> Set (Label t)
freeSet names obj = Set.filter notConstructor (freeIn obj `exceptNames` names)

closeDefs :: ObjectList -> ObjectList
closeDefs objs = uncurry app (evalWS0 (traverse closed objs))
 where
  app objs1 args
    | null (snd =<< args) =
        objs1
    | otherwise =
        closeDefs (foldr (uncurry (fmap . fmap <$$> applyArgs)) objs1 args)
  names =
    Set.fromList (objectName <$> objs)
  closed obj = do
    let extra = Set.toList (freeSet names obj)
    case obj of
      OFunction name lls expr -> do
        tell [(name, extra)]
        pure (OFunction name (extra <> lls) expr)
      OConstant name expr -> do
        tell [(name, extra)]
        pure (OFunction name extra expr)
      OExternal name t ->
        pure (OExternal name t)

applyArgs :: Name -> [Label Type] -> Expr Type -> Expr Type
applyArgs _ [] = id
applyArgs name (a : as) =
  flattenEApp
    >>> cata
      ( \case
          Core.EVar (Label t n)
            | name == n -> do
                let expr = Core.var (Label (Core.foldType t (Core.typeOf <$> (a : as))) n)
                Core.app t expr (Core.var <$> a :| as)
            | otherwise ->
                Core.var (Label t n)
          e ->
            embed e
      )

-------------------------------------------------------------------------------

addImplicitArgs :: Object Type (Expr Type) -> Object Type (Expr Type)
addImplicitArgs =
  \case
    f@(OFunction name lls1 expr)
      | isExprFun ->
          OFunction
            name
            (lls1 <> lls2)
            (flattenEApp (Core.app (List1.last ts) expr (exprs lls2)))
      | otherwise ->
          f
     where
      isExprFun =
        length ts > 1
      ts =
        Core.unfoldType (typeOf expr)
      lls2 =
        labels (List1.init ts)
    o ->
      o

exprs :: [Label t] -> List1 (Expr t)
exprs (ll : lls) = Core.var <$> ll :| lls
exprs _ = error "Implementation error"

labels :: [a] -> [Label a]
labels ts = zipWith Label ts ["$extra." <> showt i | i <- [0 :: Int ..]]

-------------------------------------------------------------------------------

sortMatchClauses :: Expr t -> Expr t
sortMatchClauses =
  cata $
    \case
      EMat t e1 cs ->
        Core.match t e1 (List1.sortBy clauseOrder cs)
      e ->
        embed e
 where
  clauseOrder (Clause (a :| _) _) (Clause (b :| _) _) =
    compare (labelName a) (labelName b)

-------------------------------------------------------------------------------

muteTypes :: Expr Type -> Expr ()
muteTypes =
  cata $
    \case
      EVar (Label _ name) ->
        Core.var (Label () name)
      ELet vs e ->
        Core.let_ (overBindingLabel muteLabelTypes <$> vs) e
      ELit p ->
        Core.lit p
      ELam lls e ->
        Core.lam (muteLabelTypes <$> lls) e
      EApp _ a es ->
        Core.app () a es
      EIf e1 e2 e3 ->
        Core.if_ e1 e2 e3
      EOp op ->
        Core.op op
      EMat _ e1 cs ->
        Core.match () e1 (muteClauseTypes <$> cs)
      EExt ll e1 e2 ->
        Core.ext (muteLabelTypes ll) e1 e2
      ENil ->
        Core.nil
      ESel (Focus name ll1 ll2) e1 e2 ->
        Core.sel (Focus name (muteLabelTypes ll1) (muteLabelTypes ll2)) e1 e2
      ECall ll es e ->
        Core.call (muteLabelTypes ll) es e
      EMem e ->
        Core.mem e

muteClauseTypes :: Clause Type (Expr ()) -> Clause () (Expr ())
muteClauseTypes (Clause lls e) = Clause (muteLabelTypes <$> lls) e

muteLabelTypes :: Label Type -> Label ()
muteLabelTypes (Label _ name) = Label () name

muteObjectTypes :: Object Type (Expr Type) -> Object () (Expr ())
muteObjectTypes =
  \case
    OFunction name lls e ->
      OFunction name (muteLabelTypes <$> lls) (muteTypes e)
    OConstant name e ->
      OConstant name (muteTypes e)
    OExternal name _ ->
      OExternal name ()

-------------------------------------------------------------------------------

data PipelineState = PipelineState
  { pipelineStateSupply :: Int
  , pipelineStateInterpreterEnv :: IRInterpreterEnv
  , pipelineStateArtifacts :: [IRInterpreterArtifact]
  , pipelineStateCode :: [IRConstruct [IRLine]]
  }
  deriving (Show, Eq, Ord)

{-# INLINE initialPipelineState #-}
initialPipelineState :: PipelineState
initialPipelineState = PipelineState 0 (IRInterpreterEnv mempty mempty) [] []

{-# INLINE overPipelineStateSupply #-}
overPipelineStateSupply :: Over PipelineState Int
overPipelineStateSupply f PipelineState{..} = PipelineState{pipelineStateSupply = f pipelineStateSupply, ..}

{-# INLINE overPipelineStateInterpreterEnv #-}
overPipelineStateInterpreterEnv :: Over PipelineState IRInterpreterEnv
overPipelineStateInterpreterEnv f PipelineState{..} = PipelineState{pipelineStateInterpreterEnv = f pipelineStateInterpreterEnv, ..}

{-# INLINE overPipelineStateInterpreterValueEnv #-}
overPipelineStateInterpreterValueEnv :: Over PipelineState (Environment IRValue)
overPipelineStateInterpreterValueEnv = overPipelineStateInterpreterEnv . inValueEnv

{-# INLINE overPipelineStateInterpreterConstructorEnv #-}
overPipelineStateInterpreterConstructorEnv :: Over PipelineState (Environment Int)
overPipelineStateInterpreterConstructorEnv = overPipelineStateInterpreterEnv . inConstructorEnv

{-# INLINE overPipelineStateArtifacts #-}
overPipelineStateArtifacts :: Over PipelineState [IRInterpreterArtifact]
overPipelineStateArtifacts f PipelineState{..} = PipelineState{pipelineStateArtifacts = f pipelineStateArtifacts, ..}

{-# INLINE overPipelineStateCode #-}
overPipelineStateCode :: Over PipelineState [IRConstruct [IRLine]]
overPipelineStateCode f PipelineState{..} = PipelineState{pipelineStateCode = f pipelineStateCode, ..}

{-# INLINE extendInterpreterValueEnv #-}
extendInterpreterValueEnv :: Environment IRValue -> Core ()
extendInterpreterValueEnv env = modify (overPipelineStateInterpreterValueEnv (<> env))

{-# INLINE extendInterpreterConstructorEnv #-}
extendInterpreterConstructorEnv :: Environment Int -> Core ()
extendInterpreterConstructorEnv env = modify (overPipelineStateInterpreterConstructorEnv (<> env))

{-# INLINE pipelineStateInsertArtifacts #-}
pipelineStateInsertArtifacts :: [IRInterpreterArtifact] -> Core ()
pipelineStateInsertArtifacts = modify . (overPipelineStateArtifacts . (<>))

{-# INLINE pipelineStateInsertCode #-}
pipelineStateInsertCode :: [IRConstruct [IRLine]] -> Core ()
pipelineStateInsertCode = modify . (overPipelineStateCode . (<>))

newtype Core a = Core {pipelineStack :: State PipelineState a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState PipelineState
    )

runCore :: Core a -> (a, PipelineState)
runCore p = runState (pipelineStack p) initialPipelineState

evalCore :: Core a -> a
evalCore p = evalState (pipelineStack p) initialPipelineState

transSuffixMonad :: (MonadState PipelineState m) => State Int a -> m a
transSuffixMonad a = do
  (v, n) <- gets (runState a . pipelineStateSupply)
  modify (overPipelineStateSupply (const n))
  pure v

transInterpreter :: IRInterpreter a -> Core a
transInterpreter p = do
  env <- gets pipelineStateInterpreterEnv
  let (a, s, _) = runInterpreter env p
  pipelineStateInsertArtifacts (irInterpreterStateArtifacts s)
  pure a

suffixNamesC :: ObjectList -> Core ObjectList
suffixNamesC = transSuffixMonad . traverse2 transSuffixExpr

pure1 :: (Applicative f) => (a -> b) -> a -> f b
pure1 f = pure . f

pure2 :: (Applicative f1, Functor f2) => (a -> b) -> f2 a -> f1 (f2 b)
pure2 f = pure . (f <$>)

pure3 :: (Applicative f1, Functor f2, Functor f3) => (a -> b) -> f2 (f3 a) -> f1 (f2 (f3 b))
pure3 f = pure . (f <$$>)

collectObjs :: (Expr Type -> Writer [Binding Type (Expr Type)] (Expr Type)) -> [Object Type (Expr Type)] -> Core [Object Type (Expr Type)]
collectObjs f as = pure (xs <> fmap toObject ys)
 where
  (xs, ys) = runWriter (traverse2 f as)

pipeline :: ObjectList -> Core [Object Type (Expr Type)]
pipeline ol = do
  a0 <- pure3 sortMatchClauses ol
  a1 <- suffixNamesC a0
  a2 <- pure3 flattenELam a1
  a3 <- collectObjs transLetLifting a2
  a4 <- collectObjs memoize a3
  a5 <- pure1 liftLambdas a4
  a6 <- pure3 simplifyELet a5
  a7 <- pure1 closeDefs a6
  pure (addImplicitArgs <$> a7)

compile :: ObjectList -> Core ()
compile ol = do
  objs <- pipeline ol
  extendInterpreterValueEnv (objectEnvironment objs)
  -- TODO
  extendInterpreterConstructorEnv (Environment.fromList [("$Cons", 0), ("$Nil", 1), ("$Record", 0), ("EqualTo", 0), ("GreaterThan", 1), ("LessThan", 2), ("Node", 1), ("Leaf", 0)])
  code <- transInterpreter (traverse objectInterpreter objs)
  pipelineStateInsertCode code

-- xx1 objs = mapM_ print $ muteObjectTypes <$> liftLambdas (flattenELam <$$> evalState (traverse (traverse transSuffixExpr) objs) 0)

xx2 objs = runCore (pipeline objs)

xx3 objs = muteObjectTypes <$> fst (xx2 objs)
