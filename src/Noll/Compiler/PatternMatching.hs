{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.PatternMatching where

import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState, State, evalState)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Noll.Common.List1 (List1, NonEmpty (..), fromList1)
import Noll.Common.Supply (supplied, suppliedName, supplyN)
import Noll.Compiler.Transform.Tree (rename, replaceWith)
import Noll.Label (Label (..))
import Noll.Language (
  BinaryOperator (..),
  Binding (..),
  Choice (..),
  Clause (..),
  CompiledClause (..),
  Constant (..),
  Expression (..),
  Function (..),
  HasType (..),
  Intrinsic (..),
  Module (..),
  Object (..),
  Pattern (..),
  Type (..),
  (~>),
 )
import Noll.Utils (
  Dictionary (..),
  Name,
  const2,
  foldrM,
  groupByEq,
  (<$$>),
 )

import qualified Data.Text as Text

data EnvelopeClause e t = EnvelopeClause (Label t) [Label t] (EnvelopeExpression e t)
  deriving (Show, Eq, Ord, Read)

data EnvelopePattern e t
  = MConstructor (Label t) [EnvelopePattern e t]
  | MVariable (Label t)
  | MLiteral t (e t)
  deriving (Show, Eq, Ord, Read)

data EnvelopeExpression e t
  = MFail
  | MExpression (e t)
  | MCase (Label t) [EnvelopeClause e t]
  | MConditional (Label t) (e t) (EnvelopeExpression e t) (EnvelopeExpression e t)
  deriving (Show, Eq, Ord, Read)

class EnvelopeHost e t where
  replace :: Name -> Name -> e t -> e t

instance (Ord t) => EnvelopeHost (Expression a) t where
  replace = rename

instance (EnvelopeHost a t, Ord t) => EnvelopeHost (EnvelopeClause a) t where
  replace var new (EnvelopeClause l1 ls e) =
    EnvelopeClause l1 ls (replace var new e)

instance (EnvelopeHost a t, Ord t) => EnvelopeHost (EnvelopeExpression a) t where
  replace _ _ MFail =
    MFail
  replace var new (MExpression e) =
    MExpression (replace var new e)

instance (HasType o k (e t)) => HasType o k (EnvelopeClause e t) where
  typeOf =
    \case
      EnvelopeClause _ _ t ->
        typeOf t

instance (HasType o k (e t)) => HasType o k (EnvelopeExpression e t) where
  typeOf =
    \case
      MFail ->
        error "TODO"
      MExpression t ->
        typeOf t
      MCase _ [] ->
        error "Implementation error"
      MCase _ (t : _) ->
        typeOf t
      MConditional _ _ t _ ->
        typeOf t

{-# INLINE fails #-}
fails :: EnvelopeClause e t -> Bool
fails (EnvelopeClause _ _ MFail) = True
fails EnvelopeClause{} = False

--

data PatternEquationBody e t = PatternEquationBody [EnvelopePattern e t] (EnvelopeExpression e t)
  deriving (Show, Eq, Ord, Read)

data HeadLiteralEquation e t = HeadLiteralEquation (e t) (PatternEquationBody e t)
  deriving (Show, Eq, Ord, Read)

data HeadVariableEquation e t = HeadVariableEquation (Label t) (PatternEquationBody e t)
  deriving (Show, Eq, Ord, Read)

data HeadConstructorEquation e t = HeadConstructorEquation (Label t) [EnvelopePattern e t] (PatternEquationBody e t)
  deriving (Show, Eq, Ord, Read)

{-# INLINE headConstructorName #-}
headConstructorName :: HeadConstructorEquation e t -> Name
headConstructorName (HeadConstructorEquation (Label _ name) _ _) = name

data PatternEquation e t
  = HeadLiteral (HeadLiteralEquation e t)
  | HeadVariable (HeadVariableEquation e t)
  | HeadConstructor (HeadConstructorEquation e t)
  | EmptyEquation (EnvelopeExpression e t)
  deriving (Show, Eq, Ord, Read)

patternEquation :: [EnvelopePattern e t] -> EnvelopeExpression e t -> PatternEquation e t
patternEquation [] ee = EmptyEquation ee
patternEquation (p : ps) ee =
  case p of
    MConstructor name qs ->
      HeadConstructor (HeadConstructorEquation name qs body)
    MVariable name ->
      HeadVariable (HeadVariableEquation name body)
    MLiteral _ e ->
      HeadLiteral (HeadLiteralEquation e body)
 where
  body = PatternEquationBody ps ee

maybeHeadLiteralEquation :: PatternEquation e t -> Maybe (HeadLiteralEquation e t)
maybeHeadLiteralEquation =
  \case
    HeadLiteral eq -> Just eq
    _ -> Nothing

maybeHeadVariableEquation :: PatternEquation e t -> Maybe (HeadVariableEquation e t)
maybeHeadVariableEquation =
  \case
    HeadVariable eq -> Just eq
    _ -> Nothing

maybeHeadConstructorEquation :: PatternEquation e t -> Maybe (HeadConstructorEquation e t)
maybeHeadConstructorEquation =
  \case
    HeadConstructor eq -> Just eq
    _ -> Nothing

maybeEmptyEquation :: PatternEquation e t -> Maybe (EnvelopeExpression e t)
maybeEmptyEquation =
  \case
    EmptyEquation ee -> Just ee
    _ -> Nothing

data PatternEquationType
  = HeadLiteralType
  | HeadVariableType
  | HeadConstructorType
  | EmptyEquationType
  deriving (Show, Eq, Ord, Read)

{-# INLINE equationType #-}
equationType :: PatternEquation e t -> PatternEquationType
equationType =
  \case
    HeadLiteral{} ->
      HeadLiteralType
    HeadVariable{} ->
      HeadVariableType
    HeadConstructor{} ->
      HeadConstructorType
    EmptyEquation{} ->
      EmptyEquationType

{-# INLINE equationGroups #-}
equationGroups :: [PatternEquation e t] -> [[PatternEquation e t]]
equationGroups = groupByEq equationType

data PatternEquationSet e t
  = AllHeadLiteral [HeadLiteralEquation e t]
  | AllHeadVariable [HeadVariableEquation e t]
  | AllHeadConstructor [HeadConstructorEquation e t]
  | AllEmpty [EnvelopeExpression e t]
  | Mixed [[PatternEquation e t]]
  deriving (Show, Eq, Ord, Read)

patternEquationSet :: [PatternEquation e t] -> PatternEquationSet e t
patternEquationSet equations =
  case equationGroups equations of
    [eqs] ->
      case ( mapMaybe maybeHeadLiteralEquation eqs
           , mapMaybe maybeHeadVariableEquation eqs
           , mapMaybe maybeHeadConstructorEquation eqs
           , mapMaybe maybeEmptyEquation eqs
           ) of
        (qs, [], [], []) ->
          AllHeadLiteral qs
        ([], qs, [], []) ->
          AllHeadVariable qs
        ([], [], qs, []) ->
          AllHeadConstructor qs
        ([], [], [], ees) ->
          AllEmpty ees
        _ ->
          error "Compiler error"
    eqss ->
      Mixed eqss

groupByHeadConstructor :: [HeadConstructorEquation e t] -> [[HeadConstructorEquation e t]]
groupByHeadConstructor = groupByEq headConstructorName . sortBy (compare `on` headConstructorName)

--

newtype MatchMonad a = MatchMonad {matchMonadStack :: ReaderT Name (State Int) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState Int
    , MonadReader Name
    )

runMatchMonad :: Name -> Int -> MatchMonad a -> a
runMatchMonad name n e = evalState (runReaderT (matchMonadStack e) name) n

type MatchRule p e t = [Label t] -> [p e t] -> EnvelopeExpression e t -> MatchMonad (EnvelopeExpression e t)

matchPatterns :: (Ord t, EnvelopeHost e t) => MatchRule PatternEquation e t
matchPatterns us qs e =
  case patternEquationSet qs of
    AllEmpty ees ->
      emptyRule us ees e
    AllHeadLiteral eqs ->
      literalRule us eqs e
    AllHeadVariable eqs ->
      variableRule us eqs e
    AllHeadConstructor eqs ->
      constructorRule us eqs e
    Mixed eqss ->
      foldrM (matchPatterns us) e eqss

emptyRule :: (EnvelopeHost e t) => MatchRule EnvelopeExpression e t
emptyRule us eqs e =
  case eqs of
    (MFail : es) ->
      emptyRule us es e
    (e1 : _) ->
      pure e1
    [] ->
      pure e

literalRule :: (Ord t, EnvelopeHost e t) => MatchRule HeadLiteralEquation e t
literalRule [] _ _ = error "Implementation error"
literalRule (u : us) eqs ex = foldrM go ex eqs
 where
  go (HeadLiteralEquation lit (PatternEquationBody qs e)) e1 = do
    e2 <- matchPatterns us [patternEquation qs e] e1
    pure (MConditional u lit e2 e1)

variableRule :: (Ord t, EnvelopeHost e t) => MatchRule HeadVariableEquation e t
variableRule [] _ _ = error "Implementation error"
variableRule (Label _ u : us) eqs ex = matchPatterns us (updateEq <$> eqs) ex
 where
  updateEq (HeadVariableEquation (Label _ name) (PatternEquationBody ps e)) =
    patternEquation ps (replace name u e)

constructorRule :: (Ord t, EnvelopeHost e t) => MatchRule HeadConstructorEquation e t
constructorRule [] _ _ = error "Implementation error"
constructorRule (u@(Label t _) : us) eqs ex = do
  cs <- traverse processGroup (groupByHeadConstructor eqs)
  pure (MCase u (cs <> [EnvelopeClause (Label t "_") [] ex]))
 where
  processGroup qs@(HeadConstructorEquation con ps _ : _) = do
    ns <- supplyN (length ps)
    let vs = uncurry freshVar <$> zip ns ps
    EnvelopeClause con vs <$> matchPatterns (vs <> us) (shift <$> qs) ex
  shift (HeadConstructorEquation _ ps (PatternEquationBody qs e)) =
    patternEquation (ps <> qs) e

-- TODO: ??
freshVar :: Int -> EnvelopePattern e t -> Label t
freshVar n =
  \case
    MVariable (Label t name) ->
      Label t (prefix <> ":" <> name)
    MConstructor (Label t _) _ ->
      Label t prefix
    MLiteral t _ ->
      Label t prefix
 where
  prefix = Text.pack ("$p:" <> show n)

--

class TypeProxy t where
  expressionType :: Expression a t -> t
  patternType :: Pattern a t -> t
  envelopeType :: EnvelopeExpression (Expression a) t -> t
  arrow :: t -> t -> t
  boolean :: t

instance TypeProxy () where
  expressionType =
    const ()
  patternType =
    const ()
  envelopeType =
    const ()
  arrow =
    const2 ()
  boolean =
    ()

instance TypeProxy (Type o k) where
  expressionType =
    typeOf
  patternType =
    typeOf
  envelopeType =
    typeOf
  arrow =
    TArrow
  boolean =
    TIntrinsic IBool

compileEnvelope :: (TypeProxy t, EnvelopeHost (Expression a) t, Monoid a, Eq t) => EnvelopeExpression (Expression a) t -> Expression a t
compileEnvelope =
  \case
    MFail ->
      error "Pattern matching failure"
    MExpression expr ->
      expr
    e@(MCase ll cs) ->
      ECompiledMatch mempty (envelopeType e) (EVariable mempty ll) (clauseList cs)
    MConditional ll e1 e2 e3 ->
      EIf
        mempty
        (envelopeType e2)
        ( EApplication
            mempty
            boolean
            (EBinaryOperator mempty (expressionType e1 `arrow` expressionType e1 `arrow` boolean, OEqualTo))
            (EVariable mempty ll :| [e1])
        )
        (compileEnvelope e2)
        (compileEnvelope e3)

compileEnvelopeClause :: (TypeProxy t, EnvelopeHost (Expression a) t, Monoid a, Eq t) => EnvelopeClause (Expression a) t -> CompiledClause Expression a t
compileEnvelopeClause (EnvelopeClause l1 ls e) = ECompiledClause (l1 :| ls) (compileEnvelope e)

clauseList :: (TypeProxy t, EnvelopeHost (Expression a) t, Monoid a, Eq t) => [EnvelopeClause (Expression a) t] -> List1 (CompiledClause Expression a t)
clauseList ecs =
  case filter (not . fails) ecs of
    c : cs ->
      compileEnvelopeClause <$> (c :| cs)
    [] ->
      error "Implementation error"

--

class MatchExpressionContext a where
  compileMatchExprs :: a -> MatchMonad a

instance (MatchExpressionContext a) => MatchExpressionContext [a] where
  compileMatchExprs = traverse compileMatchExprs

instance (MatchExpressionContext a) => MatchExpressionContext (Maybe a) where
  compileMatchExprs = traverse compileMatchExprs

instance (MatchExpressionContext a) => MatchExpressionContext (List1 a) where
  compileMatchExprs = traverse compileMatchExprs

instance (MatchExpressionContext a) => MatchExpressionContext (Dictionary a) where
  compileMatchExprs = traverse compileMatchExprs

instance (Show a, Show t, TypeProxy t, Ord t, Monoid a) => MatchExpressionContext (Module a k t) where
  compileMatchExprs =
    \case
      Module p ns os ->
        Module p ns <$> compileMatchExprs os

instance (Show a, Show t, TypeProxy t, Ord t, Monoid a) => MatchExpressionContext (Object a k t) where
  compileMatchExprs =
    \case
      DFunction name f ->
        DFunction name <$> compileMatchExprs f
      DConstant name c ->
        DConstant name <$> compileMatchExprs c

instance (MatchExpressionContext (e a t)) => MatchExpressionContext (Function e a t) where
  compileMatchExprs =
    \case
      Function a u ps e ->
        Function a u ps <$> compileMatchExprs e

instance (MatchExpressionContext (e a t)) => MatchExpressionContext (Constant e a t) where
  compileMatchExprs =
    \case
      Constant a u e ->
        Constant a u <$> compileMatchExprs e

instance MatchExpressionContext (Clause Expression a t) where
  compileMatchExprs =
    \case
      EClause a p cs ->
        pure (EClause a p cs)

instance (Show a, Show t, TypeProxy t, Ord t, Monoid a) => MatchExpressionContext (Binding Expression a t) where
  compileMatchExprs =
    \case
      BPattern a p e ->
        BPattern a p <$> compileMatchExprs e

instance (Show a, Show t, TypeProxy t, Ord t, Monoid a) => MatchExpressionContext (Expression a t) where
  compileMatchExprs =
    \case
      EAnnotation a t e ->
        EAnnotation a t <$> compileMatchExprs e
      EMatch a t e cs -> do
        cs1 <- compileMatchExprs cs
        name <- suppliedName
        replaceWith name <$> compileMatchExprs e <*> compileClauses (Label (expressionType e) name) cs1
      ELambda a ps e ->
        ELambda a ps <$> compileMatchExprs e
      ERecursiveLet a p e1 e2 ->
        ERecursiveLet a p <$> compileMatchExprs e1 <*> compileMatchExprs e2
      ELet a gs e1 ->
        ELet a <$> compileMatchExprs gs <*> compileMatchExprs e1
      EIf a t e1 e2 e3 ->
        EIf a t <$> compileMatchExprs e1 <*> compileMatchExprs e2 <*> compileMatchExprs e2
      EApplication a t e1 es ->
        EApplication a t <$> compileMatchExprs e1 <*> compileMatchExprs es
      EListCons a t e1 e2 ->
        EListCons a t <$> compileMatchExprs e1 <*> compileMatchExprs e2
      EListLiteral a t es ->
        EListLiteral a t <$> compileMatchExprs es
      ERecord a t d e ->
        ERecord a t <$> compileMatchExprs d <*> compileMatchExprs e
      ESelect a ll e ->
        undefined
      EFold a t es cs e ->
        undefined
      e@EUnaryOperator{} ->
        pure e
      e@EBinaryOperator{} ->
        pure e
      e@EVariable{} ->
        pure e
      e@EConstructor{} ->
        pure e
      e@ELiteral{} ->
        pure e

compileClauses :: (Show a, Show t, TypeProxy t, Monoid a, Ord t) => Label t -> List1 (Clause Expression a t) -> MatchMonad (Expression a t)
compileClauses ll cs = compileEnvelope <$> matchPatterns [ll] eqs MFail
 where
  eqs = uncurry patternEquation . translateClause <$> fromList1 cs

translateClause :: (Show a, Show t, TypeProxy t) => Clause Expression a t -> ([EnvelopePattern (Expression a) t], EnvelopeExpression (Expression a) t)
translateClause (EClause _ p (CPlain _ _ e :| [])) =
  ([translatePattern p], MExpression e)
translateClause _ =
  error "TODO"

translatePattern :: (Show a, Show t, TypeProxy t) => Pattern a t -> EnvelopePattern (Expression a) t
translatePattern =
  \case
    PVariable _ ll ->
      MVariable ll
    PConstructor _ ll ps ->
      MConstructor ll (translatePattern <$> ps)
    p@(PLiteral a prim) ->
      MLiteral (patternType p) (ELiteral a prim)
