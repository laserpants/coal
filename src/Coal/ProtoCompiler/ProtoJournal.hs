{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoJournal (
  ProtoCompilerJournal (..),
  ProtoError (..),
  tellErrors,
) where

import Coal.ProtoCompiler.ProtoError (ProtoError (..))
import Control.Monad.Writer (MonadWriter, tell)

data ProtoCompilerJournal a = ProtoCompilerJournal
  { protoOcompilerJournalErrors :: [ProtoError]
  }
  deriving (Show, Eq)

instance Semigroup (ProtoCompilerJournal a) where
  (<>) (ProtoCompilerJournal a1) (ProtoCompilerJournal b1) =
    ProtoCompilerJournal
      (a1 <> b1)

instance Monoid (ProtoCompilerJournal a) where
  mempty = ProtoCompilerJournal []

{-# INLINE tellErrors #-}
tellErrors :: (MonadWriter (ProtoCompilerJournal a) m) => [ProtoError] -> m ()
tellErrors w = tell $ ProtoCompilerJournal w
