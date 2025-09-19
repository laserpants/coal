{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Journal (
  CompilerJournal (..),
  tellPatterns,
  tellPatterns1,
  listenPatterns,
) where

import Coal.Language
import Control.Monad.Writer (MonadWriter, listen, tell)
import Data.Tuple.Extra (second)
import Extra (Name)

data CompilerJournal a = CompilerJournal
  { compilerJournalPatterns :: [(Name, Pattern a IndexedType)]
  }
  deriving (Show, Eq, Ord, Read)

instance Semigroup (CompilerJournal a) where
  (<>) (CompilerJournal a1) (CompilerJournal b1) =
    CompilerJournal (a1 <> b1)

instance Monoid (CompilerJournal a) where
  mempty = CompilerJournal []

{-# INLINE tellPatterns #-}
tellPatterns :: (MonadWriter (CompilerJournal a) m) => [(Name, Pattern a IndexedType)] -> m ()
tellPatterns a = tell $ CompilerJournal a

{-# INLINE tellPatterns1 #-}
tellPatterns1 :: (MonadWriter (CompilerJournal a) m) => (Name, Pattern a IndexedType) -> m ()
tellPatterns1 a = tellPatterns [a]

listenPatterns :: (MonadWriter (CompilerJournal a) m) => m e -> m (e, [(Name, Pattern a IndexedType)])
listenPatterns a = second compilerJournalPatterns <$> listen a
