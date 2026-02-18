{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoJournal (
  ProtoCompilerJournal (..),
  ProtoError (..),
  tellErrors,
  protoOcompilerReportConstraintsGenErrors,
) where

import Coal.ProtoCompiler.ProtoError (ProtoError (..))
import Coal.TypeSystem.Constraint.Generation.Error (ConstraintsGenError (..))
import Control.Monad.Writer (MonadWriter, tell)

data ProtoCompilerJournal a = ProtoCompilerJournal
  { protoOcompilerJournalErrors :: [ProtoError]
  , protoOcompilerConstraintsGenErrors :: [ConstraintsGenError a]
  }
  deriving (Show, Eq)

instance Semigroup (ProtoCompilerJournal a) where
  (<>) (ProtoCompilerJournal a1 a2) (ProtoCompilerJournal b1 b2) =
    ProtoCompilerJournal
      (a1 <> b1)
      (a2 <> b2)

instance Monoid (ProtoCompilerJournal a) where
  mempty = ProtoCompilerJournal [] []

{-# INLINE tellErrors #-}
tellErrors :: (MonadWriter (ProtoCompilerJournal a) m) => [ProtoError] -> m ()
tellErrors w = tell $ ProtoCompilerJournal w []

{-# INLINE protoOcompilerReportConstraintsGenErrors #-}
protoOcompilerReportConstraintsGenErrors :: (MonadWriter (ProtoCompilerJournal a) m) => [ConstraintsGenError a] -> m ()
protoOcompilerReportConstraintsGenErrors errs = tell $ ProtoCompilerJournal [] errs
