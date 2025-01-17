{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.PatternBindingExpansion where

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
import Noll.Language.Pattern (Pattern (..))
import Noll.Language.Tagged (Tagged (..))
import Noll.Language.Type (Type (..))
import Noll.Utils (Name, foldrM)

import qualified Data.Text as Text

type NamedPattern a o k = (Name, Pattern a (Type o k))

runTranslatable :: Name -> Int -> WriterT [NamedPattern a o k] (ReaderT Name (State Int)) e -> e
runTranslatable r s e = fst $ evalState (runReaderT (runWriterT e) r) s

class Translatable a o k e | e -> a, e -> o k where
  translate :: (MonadWriter [NamedPattern a o k] m, MonadReader Name m, MonadState Int m) => e -> m e

instance Translatable a o k (Pattern a (Type o k)) where
  translate =
    \case
      p@PVariable{} ->
        pure p
      p@(PAnnotation _ _ PVariable{}) ->
        pure p
      p -> do
        name <- freshVar
        tell [(name, p)]
        pure (PVariable (tag p) (Label (typeOf p) name))

freshVar :: (MonadWriter [NamedPattern a o k] m, MonadReader Name m, MonadState Int m) => m Name
freshVar = do
  prefix <- ask
  n <- supplied id
  pure (Text.pack ("$" <> Text.unpack prefix <> "." <> show n))

instance Translatable a o k (Binding Expression a (Type o k)) where
  translate =
    \case
      BPattern a p e ->
        BPattern a <$> translate p <*> translate e
      BFunction a name ps e -> do
        BFunction a name <$> traverse translate ps <*> translate e

instance Translatable a o k (Expression a (Type o k)) where
  translate =
    \case
      EAnnotation a t e ->
        EAnnotation a t <$> translate e
      EApplication a t e1 es ->
        EApplication a t <$> translate e1 <*> traverse translate es
      EIf a t e1 e2 e3 ->
        EIf a t <$> translate e1 <*> translate e2 <*> translate e3
      ERecord a t d e ->
        ERecord a t <$> traverse translate d <*> traverse translate e
      EListCons a t e1 e2 ->
        EListCons a t <$> translate e1 <*> translate e2
      EListLiteral a t es ->
        EListLiteral a t <$> traverse translate es
      EMatch a t e cs ->
        EMatch a t <$> translate e <*> pure cs
      ESelect a (Label t name) e ->
        ESelect a (Label t name) <$> translate e
      EFold a t es cs e ->
        EFold a t <$> traverse translate es <*> pure cs <*> traverse translate e
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
        e2 <- translate e1
        (hs, ps) <- runWriterT (traverse translate gs)
        pure $ ELet a hs (foldr unrollMatch e2 ps)
      ERecursiveLet a p e1 e2 -> do
        (p1, ps) <- runWriterT (translate p)
        q1 <- BPattern a p1 <$> translate e1
        pure $ ELet a (q1 :| []) (foldr unrollMatch e2 ps)
      ELambda a ps e -> do
        e1 <- translate e
        (qs, ps) <- runWriterT (traverse translate ps)
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
