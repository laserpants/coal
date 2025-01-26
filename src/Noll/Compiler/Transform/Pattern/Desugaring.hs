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
import Noll.Common.Supply (suppliedName)
import Noll.Compiler.Transform.Expression (mapMOverExpression)
import Noll.Label (Label (..))
import Noll.Language.Expression (Clause (..), Expression (..))
import Noll.Language.Expression.Binding (Binding (..))
import Noll.Language.Expression.Choice (Choice (..))
import Noll.Language.HasType (HasType (..))
import Noll.Language.Module.Constant (Constant (..))
import Noll.Language.Module.Definition (Definition (..))
import Noll.Language.Module.Function (Function (..))
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
  desugarPatterns ::
    (MonadWriter [NamedPattern c o k] m, MonadReader Name m, MonadState Int m) =>
    e ->
    m e

instance (Monoid c) => Sugared c o k (Pattern c (Type o k)) where
  desugarPatterns =
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
  desugarPatterns =
    \case
      BPattern a p e ->
        BPattern a <$> desugarPatterns p <*> desugarPatterns e
      BFunction a name ps e ->
        BFunction a name <$> traverse desugarPatterns ps <*> desugarPatterns e

instance (Monoid c) => Sugared c o k (Expression c (Type o k)) where
  desugarPatterns =
    mapMOverExpression $
      \case
        ELet a gs e1 -> do
          e2 <- desugarPatterns e1
          (hs, ps) <- runWriterT (traverse desugarPatterns gs)
          pure (ELet a hs (foldr unrollMatch e2 ps))
        ERecursiveLet a p e1 e2 -> do
          (p1, ps) <- runWriterT (desugarPatterns p)
          q1 <- BPattern a p1 <$> desugarPatterns e1
          pure (ELet a (q1 :| []) (foldr unrollMatch e2 ps))
        ELambda a ps e -> do
          e1 <- desugarPatterns e
          (qs, ps) <- runWriterT (traverse desugarPatterns ps)
          pure (ELambda a qs (foldr unrollMatch e1 ps))
        e ->
          pure e

unrollMatch :: (Monoid c) => (Name, Pattern c (Type o k)) -> Expression c (Type o k) -> Expression c (Type o k)
unrollMatch (name, p) e =
  EMatch
    mempty
    (typeOf e)
    (EVariable mempty (Label (typeOf p) name))
    (EClause mempty p (CPlain mempty [] e :| []) :| [])

instance (Monoid c) => Sugared c o k (Function Expression c (Type o k)) where
  desugarPatterns =
    \case
      Function a u ps e -> do
        e1 <- desugarPatterns e
        (qs, ps) <- runWriterT (traverse desugarPatterns ps)
        pure (Function a u qs (foldr unrollMatch e1 ps))

instance (Monoid c) => Sugared c o k (Constant Expression c (Type o k)) where
  desugarPatterns =
    \case
      Constant a u e ->
        Constant a u <$> desugarPatterns e

instance (Monoid c) => Sugared c o k (Definition c k (Type o k)) where
  desugarPatterns =
    \case
      DFunction name f ->
        DFunction name <$> desugarPatterns f
      DConstant name g ->
        DConstant name <$> desugarPatterns g
      d ->
        pure d
