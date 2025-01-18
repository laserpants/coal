{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Pattern.Expansion where

import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState, State, evalState)
import Control.Monad.Writer (MonadWriter, WriterT, runWriterT, tell)
import Noll.Common.List1 (List1, NonEmpty ((:|)))
import Noll.Common.Supply (supplied)
import Noll.Label (Label (..))
import Noll.Language.Expression (Clause (..), Expression (..))
import Noll.Language.Expression.Binding (Binding (..))
import Noll.Language.Expression.Choice (Choice (..))
import Noll.Language.HasType (HasType (..))
import Noll.Language.Module.Function (Function (..))
import Noll.Language.Module.Global (Global (..))
import Noll.Language.Module.Object (Object (..))
import Noll.Language.Pattern (Pattern (..))
import Noll.Language.Tagged (Tagged (..))
import Noll.Language.Type (Type (..))
import Noll.Utils (Name, foldrM)

import qualified Data.Text as Text

type NamedPattern a o k = (Name, Pattern a (Type o k))

type PatternExpansionStack a o k = WriterT [NamedPattern a o k] (ReaderT Name (State Int))

newtype PatternExpansion a o k e = PatternExpansion {patternExpansionStack :: PatternExpansionStack a o k e}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Name
    , MonadState Int
    , MonadWriter [NamedPattern a o k]
    )

runExpandPatterns :: Name -> Int -> PatternExpansion a o k e -> e
runExpandPatterns r s e = fst (evalState (runReaderT (runWriterT (patternExpansionStack e)) r) s)

class Expandable a o k e | e -> a, e -> o k where
  expandPatterns :: (MonadWriter [NamedPattern a o k] m, MonadReader Name m, MonadState Int m) => e -> m e

instance Expandable a o k (Pattern a (Type o k)) where
  expandPatterns =
    \case
      p@PVariable{} ->
        pure p
      p@(PAnnotation _ _ PVariable{}) ->
        pure p
      p -> do
        name <- ask >>= supplied . freshName
        tell [(name, p)]
        pure (PVariable (tag p) (Label (typeOf p) name))

{-# INLINE freshName #-}
freshName :: Name -> Int -> Name
freshName prefix index = Text.pack ("$" <> Text.unpack prefix <> "." <> show index)

instance Expandable a o k (Binding Expression a (Type o k)) where
  expandPatterns =
    \case
      BPattern a p e ->
        BPattern a <$> expandPatterns p <*> expandPatterns e
      BFunction a name ps e ->
        BFunction a name <$> traverse expandPatterns ps <*> expandPatterns e

instance Expandable a o k (Expression a (Type o k)) where
  expandPatterns =
    \case
      EAnnotation a t e ->
        EAnnotation a t <$> expandPatterns e
      EApplication a t e1 es ->
        EApplication a t <$> expandPatterns e1 <*> traverse expandPatterns es
      EIf a t e1 e2 e3 ->
        EIf a t <$> expandPatterns e1 <*> expandPatterns e2 <*> expandPatterns e3
      ERecord a t d e ->
        ERecord a t <$> traverse expandPatterns d <*> traverse expandPatterns e
      EListCons a t e1 e2 ->
        EListCons a t <$> expandPatterns e1 <*> expandPatterns e2
      EListLiteral a t es ->
        EListLiteral a t <$> traverse expandPatterns es
      EMatch a t e cs ->
        EMatch a t <$> expandPatterns e <*> pure cs
      ESelect a (Label t name) e ->
        ESelect a (Label t name) <$> expandPatterns e
      EFold a t es cs e ->
        EFold a t <$> traverse expandPatterns es <*> pure cs <*> traverse expandPatterns e
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
      ELet a gs e1 -> do
        e2 <- expandPatterns e1
        (hs, ps) <- runWriterT (traverse expandPatterns gs)
        pure $ ELet a hs (foldr unrollMatch e2 ps)
      ERecursiveLet a p e1 e2 -> do
        (p1, ps) <- runWriterT (expandPatterns p)
        q1 <- BPattern a p1 <$> expandPatterns e1
        pure $ ELet a (q1 :| []) (foldr unrollMatch e2 ps)
      ELambda a ps e -> do
        e1 <- expandPatterns e
        (qs, ps) <- runWriterT (traverse expandPatterns ps)
        pure $ ELambda a qs (foldr unrollMatch e1 ps)

unrollMatch :: (Name, Pattern a (Type o k)) -> Expression a (Type o k) -> Expression a (Type o k)
unrollMatch (name, p) e =
  EMatch
    (tag e)
    (typeOf e)
    (EVariable a (Label (typeOf p) name))
    (EClause a p (CPlain a [] e :| []) :| [])
 where
  a = tag p

instance Expandable a o k (Function Expression a (Type o k)) where
  expandPatterns =
    \case
      Function a u ps e -> do
        e1 <- expandPatterns e
        (qs, ps) <- runWriterT (traverse expandPatterns ps)
        pure $ Function a u qs (foldr unrollMatch e1 ps)

instance Expandable a o k (Global Expression a (Type o k)) where
  expandPatterns =
    \case
      Global a u e ->
        Global a u <$> expandPatterns e

instance Expandable a o k (Object a k (Type o k)) where
  expandPatterns =
    \case
      DFunction name f ->
        DFunction name <$> expandPatterns f
      DGlobal name g ->
        DGlobal name <$> expandPatterns g
      d ->
        pure d
