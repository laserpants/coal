{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Journal (
  CompilerJournal (..),
  tellPatterns,
  tellPatterns1,
  listenPatterns,
  listenWhereClauses,
  tellWhereClauses,
) where

import Coal.Language
import Control.Monad.Writer (MonadWriter, listen, tell)
import Data.Tuple.Extra (second)
import Extra (Name)

data CompilerJournal a = CompilerJournal
  { compilerJournalPatterns :: [(Name, Pattern a IndexedType)]
  , compilerJournalWhereClauses :: [(Name, Name)]
  }
  deriving (Show, Eq, Ord, Read)

instance Semigroup (CompilerJournal a) where
  (<>) (CompilerJournal a1 a2) (CompilerJournal b1 b2) =
    CompilerJournal
      (a1 <> b1)
      (a2 <> b2)

instance Monoid (CompilerJournal a) where
  mempty = CompilerJournal [] []

{-# INLINE tellPatterns #-}
tellPatterns :: (MonadWriter (CompilerJournal a) m) => [(Name, Pattern a IndexedType)] -> m ()
tellPatterns w = tell $ CompilerJournal w []

{-# INLINE tellPatterns1 #-}
tellPatterns1 :: (MonadWriter (CompilerJournal a) m) => (Name, Pattern a IndexedType) -> m ()
tellPatterns1 w = tellPatterns [w]

{-# INLINE tellWhereClauses #-}
tellWhereClauses :: (MonadWriter (CompilerJournal a) m) => [(Name, Name)] -> m ()
tellWhereClauses w = tell $ CompilerJournal [] w

listenPatterns :: (MonadWriter (CompilerJournal a) m) => m e -> m (e, [(Name, Pattern a IndexedType)])
listenPatterns w = second compilerJournalPatterns <$> listen w

listenWhereClauses :: (MonadWriter (CompilerJournal a) m) => m e -> m (e, [(Name, Name)])
listenWhereClauses w = second compilerJournalWhereClauses <$> listen w
