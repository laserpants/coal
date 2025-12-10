{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Journal (
  CompilerJournal (..),
  RecordEntry,
  tellPatterns,
  tellPatterns1,
  listenPatterns,
  listenWhereClauses,
  listenRecordEntry,
  listenDictionaryTraits,
  listenErrors,
  tellWhereClauses,
  tellRecordEntry,
  tellDictionaryTraits,
  tellErrors,
  censorDictionaryTraits,
) where

import Coal.Common.Name (Dictionary, Name)
import Coal.Compiler.Error (CompilerError (..))
import Coal.Language.Pattern (IndexedPattern, Pattern)
import Coal.Language.Trait (Trait)
import Coal.Language.Type (IndexedType)
import Control.Monad.Writer (MonadWriter, censor, listen, tell)
import Data.Tuple.Extra (second)

type RecordEntry a = (Name, Dictionary (IndexedPattern a), Maybe (IndexedPattern a))

data CompilerJournal a = CompilerJournal
  { compilerJournalPatterns :: [(Name, Pattern a IndexedType)]
  , compilerJournalWhereClauses :: [(Name, Name)]
  , compilerJournalRecordEntries :: [RecordEntry a]
  , compilerJournalDictionaryTraits :: [Trait IndexedType]
  , compilerJournalErrors :: [CompilerError a]
  }
  deriving (Show, Eq)

instance Semigroup (CompilerJournal a) where
  (<>) (CompilerJournal a1 a2 a3 a4 a5) (CompilerJournal b1 b2 b3 b4 b5) =
    CompilerJournal
      (a1 <> b1)
      (a2 <> b2)
      (a3 <> b3)
      (a4 <> b4)
      (a5 <> b5)

instance Monoid (CompilerJournal a) where
  mempty = CompilerJournal [] [] [] [] []

{-# INLINE tellPatterns #-}
tellPatterns :: (MonadWriter (CompilerJournal a) m) => [(Name, Pattern a IndexedType)] -> m ()
tellPatterns w = tell $ CompilerJournal w [] [] [] []

{-# INLINE tellPatterns1 #-}
tellPatterns1 :: (MonadWriter (CompilerJournal a) m) => (Name, Pattern a IndexedType) -> m ()
tellPatterns1 w = tellPatterns [w]

{-# INLINE tellWhereClauses #-}
tellWhereClauses :: (MonadWriter (CompilerJournal a) m) => [(Name, Name)] -> m ()
tellWhereClauses w = tell $ CompilerJournal [] w [] [] []

{-# INLINE tellRecordEntry #-}
tellRecordEntry :: (MonadWriter (CompilerJournal a) m) => [RecordEntry a] -> m ()
tellRecordEntry w = tell $ CompilerJournal [] [] w [] []

{-# INLINE tellDictionaryTraits #-}
tellDictionaryTraits :: (MonadWriter (CompilerJournal a) m) => [Trait IndexedType] -> m ()
tellDictionaryTraits w = tell $ CompilerJournal [] [] [] w []

{-# INLINE tellErrors #-}
tellErrors :: (MonadWriter (CompilerJournal a) m) => [CompilerError a] -> m ()
tellErrors w = tell $ CompilerJournal [] [] [] [] w

listenPatterns :: (MonadWriter (CompilerJournal a) m) => m e -> m (e, [(Name, Pattern a IndexedType)])
listenPatterns w = second compilerJournalPatterns <$> listen w

listenWhereClauses :: (MonadWriter (CompilerJournal a) m) => m e -> m (e, [(Name, Name)])
listenWhereClauses w = second compilerJournalWhereClauses <$> listen w

listenRecordEntry :: (MonadWriter (CompilerJournal a) m) => m e -> m (e, [RecordEntry a])
listenRecordEntry w = second compilerJournalRecordEntries <$> listen w

listenDictionaryTraits :: (MonadWriter (CompilerJournal a) m) => m e -> m (e, [Trait IndexedType])
listenDictionaryTraits w = second compilerJournalDictionaryTraits <$> listen w

listenErrors :: (MonadWriter (CompilerJournal a) m) => m e -> m (e, [CompilerError a])
listenErrors w = second compilerJournalErrors <$> listen w

censorDictionaryTraits :: (MonadWriter (CompilerJournal a) m) => ([Trait IndexedType] -> [Trait IndexedType]) -> m b -> m b
censorDictionaryTraits f =
  censor $
    \CompilerJournal{..} ->
      CompilerJournal
        { compilerJournalDictionaryTraits =
            f compilerJournalDictionaryTraits
        , ..
        }
