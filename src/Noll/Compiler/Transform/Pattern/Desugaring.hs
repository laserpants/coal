{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Pattern.Desugaring where

import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState, State, evalState)
import Control.Monad.Writer (MonadWriter, WriterT, runWriterT, tell)
import Noll.Common.List1 (List1, NonEmpty ((:|)))
import Noll.Common.Supply (supplied, suppliedName)
import Noll.Compiler.Transform.Expression (mapMOverExpression)
import Noll.Label (Label (..))
import Noll.Language.Expression (Clause (..), Expression (..))
import Noll.Language.Expression.Binding (Binding (..))
import Noll.Language.Expression.Choice (Choice (..))
import Noll.Language.HasType (HasType (..))
import Noll.Language.Module.Constant (Constant (..))
import Noll.Language.Module.Function (Function (..))
import Noll.Language.Module.Object (Object (..))
import Noll.Language.Pattern (Pattern (..))
import Noll.Language.Type (Type (..))
import Noll.Utils (Name, foldrM)

import qualified Data.Text as Text

type NamedPattern c o k = (Name, Pattern c (Type o k))

type PatternDesugaringStack c o k = WriterT [NamedPattern c o k] (ReaderT Name (State Int))

newtype PatternDesugaring c o k e = PatternDesugaring {patternDesugaringStack :: PatternDesugaringStack c o k e}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Name
    , MonadState Int
    , MonadWriter [NamedPattern c o k]
    )

{-# INLINE runPatternDesugaring #-}
runPatternDesugaring :: Name -> Int -> PatternDesugaring c o k e -> e
runPatternDesugaring r s e = fst (evalState (runReaderT (runWriterT (patternDesugaringStack e)) r) s)

class Sugared c o k e | e -> c, e -> o k where
  expandPatterns ::
    (MonadWriter [NamedPattern c o k] m, MonadReader Name m, MonadState Int m) =>
    e ->
    m e

instance (Monoid c) => Sugared c o k (Pattern c (Type o k)) where
  expandPatterns =
    \case
      p@PVariable{} ->
        pure p
      p@(PAnnotation _ _ PVariable{}) ->
        pure p
      p -> do
        name <- suppliedName
        tell [(name, p)]
        pure (PVariable mempty (Label (typeOf p) name))

instance (Monoid c) => Sugared c o k (Binding Expression c (Type o k)) where
  expandPatterns =
    \case
      BPattern a p e ->
        BPattern a <$> expandPatterns p <*> expandPatterns e
      BFunction a name ps e ->
        BFunction a name <$> traverse expandPatterns ps <*> expandPatterns e

instance (Monoid c) => Sugared c o k (Expression c (Type o k)) where
  expandPatterns =
    \case
      ELet a gs e1 -> do
        e2 <- expandPatterns e1
        (hs, ps) <- runWriterT (traverse expandPatterns gs)
        pure (ELet a hs (foldr unrollMatch e2 ps))
      ERecursiveLet a p e1 e2 -> do
        (p1, ps) <- runWriterT (expandPatterns p)
        q1 <- BPattern a p1 <$> expandPatterns e1
        pure (ELet a (q1 :| []) (foldr unrollMatch e2 ps))
      ELambda a ps e -> do
        e1 <- expandPatterns e
        (qs, ps) <- runWriterT (traverse expandPatterns ps)
        pure (ELambda a qs (foldr unrollMatch e1 ps))
      e ->
        mapMOverExpression expandPatterns e

unrollMatch :: (Monoid c) => (Name, Pattern c (Type o k)) -> Expression c (Type o k) -> Expression c (Type o k)
unrollMatch (name, p) e =
  EMatch
    mempty
    (typeOf e)
    (EVariable mempty (Label (typeOf p) name))
    (EClause mempty p (CPlain mempty [] e :| []) :| [])

instance (Monoid c) => Sugared c o k (Function Expression c (Type o k)) where
  expandPatterns =
    \case
      Function a u ps e -> do
        e1 <- expandPatterns e
        (qs, ps) <- runWriterT (traverse expandPatterns ps)
        pure (Function a u qs (foldr unrollMatch e1 ps))

instance (Monoid c) => Sugared c o k (Constant Expression c (Type o k)) where
  expandPatterns =
    \case
      Constant a u e ->
        Constant a u <$> expandPatterns e

instance (Monoid c) => Sugared c o k (Object c k (Type o k)) where
  expandPatterns =
    \case
      DFunction name f ->
        DFunction name <$> expandPatterns f
      DConstant name g ->
        DConstant name <$> expandPatterns g
      d ->
        pure d
