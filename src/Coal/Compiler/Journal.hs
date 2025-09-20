{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Journal (
  CompilerJournal (..),
  RecordInfo,
  tellPatterns,
  tellPatterns1,
  listenPatterns,
  listenWhereClauses,
  listenRecordInfo,
  tellWhereClauses,
  tellRecordInfo,
) where

import Coal.Language
import Control.Monad.Writer (MonadWriter, listen, tell)
import Data.Tuple.Extra (second)
import Extra (Dictionary, Name)

type RecordInfo a = (Name, Dictionary (IndexedPattern a), Maybe (IndexedPattern a))

data CompilerJournal a = CompilerJournal
  { compilerJournalPatterns :: [(Name, Pattern a IndexedType)]
  , compilerJournalWhereClauses :: [(Name, Name)]
  , compilerJournalRecordInfo :: [RecordInfo a]
  }
  deriving (Show, Eq, Ord, Read)

instance Semigroup (CompilerJournal a) where
  (<>) (CompilerJournal a1 a2 a3) (CompilerJournal b1 b2 b3) =
    CompilerJournal
      (a1 <> b1)
      (a2 <> b2)
      (a3 <> b3)

instance Monoid (CompilerJournal a) where
  mempty = CompilerJournal [] [] []

{-# INLINE tellPatterns #-}
tellPatterns :: (MonadWriter (CompilerJournal a) m) => [(Name, Pattern a IndexedType)] -> m ()
tellPatterns w = tell $ CompilerJournal w [] []

{-# INLINE tellPatterns1 #-}
tellPatterns1 :: (MonadWriter (CompilerJournal a) m) => (Name, Pattern a IndexedType) -> m ()
tellPatterns1 w = tellPatterns [w]

{-# INLINE tellWhereClauses #-}
tellWhereClauses :: (MonadWriter (CompilerJournal a) m) => [(Name, Name)] -> m ()
tellWhereClauses w = tell $ CompilerJournal [] w []

{-# INLINE tellRecordInfo #-}
tellRecordInfo :: (MonadWriter (CompilerJournal a) m) => [RecordInfo a] -> m ()
tellRecordInfo w = tell $ CompilerJournal [] [] w

listenPatterns :: (MonadWriter (CompilerJournal a) m) => m e -> m (e, [(Name, Pattern a IndexedType)])
listenPatterns w = second compilerJournalPatterns <$> listen w

listenWhereClauses :: (MonadWriter (CompilerJournal a) m) => m e -> m (e, [(Name, Name)])
listenWhereClauses w = second compilerJournalWhereClauses <$> listen w

listenRecordInfo :: (MonadWriter (CompilerJournal a) m) => m e -> m (e, [RecordInfo a])
listenRecordInfo w = second compilerJournalRecordInfo <$> listen w
