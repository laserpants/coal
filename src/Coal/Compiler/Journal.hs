{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Journal (
  CompilerJournal (..),
  RecordEntry,
  tellPatterns,
  tellPatterns1,
  listenPatterns,
  listenRecordEntry,
  listenDictionaryTraits,
  listenErrors,
  tellRecordEntry,
  tellDictionaryTraits,
  tellErrors,
  censorDictionaryTraits,
) where

import Coal.Common.Name (Dictionary, Name)
import Coal.Compiler.Error (CompilerError (..))
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Trait (Trait)
import Coal.Language.Type (IndexedType)
import Coal.Language.Type.Kind (Kind)
import Control.Monad.Writer (MonadWriter, censor, listen, pass, tell)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (second)

type RecordEntry a = (Name, Dictionary (Pattern a Kind IndexedType), Maybe (Pattern a Kind IndexedType))

data CompilerJournal a = CompilerJournal
  { compilerJournalPatterns :: [(Name, Pattern a Kind IndexedType)]
  , compilerJournalRecordEntries :: [RecordEntry a]
  , compilerJournalDictionaryTraits :: Set (Trait IndexedType)
  , compilerJournalErrors :: [CompilerError a]
  }
  deriving (Show, Eq)

{- | Journal accumulation, with the two monoid identities implemented as
identities.

'Coal.Compiler.Stack.CompilerT' is an @RWST@, so every monadic bind appends to
the journal, and the accumulated value is not consulted until the end of the
compilation. Implementing @mempty <> x = x@ and @x <> mempty = x@ directly stops
the accumulated journal from becoming a long chain of unforced @( <>)@ thunks:
nothing forces that chain until the very end, so on a large build it dominates
the live heap (a heap profile of a 42-module build shows it at ~100% of peak
residency, and the repeated forcing of the chain accounts for ~10% of compile
time).

The emptiness test is O(1) per field (@null@ on a list spine, 'Set.null'), and
because the chain is kept collapsed it stays O(1) as the journal grows. This is
sound because @isEmptyJournal a@ implies @a == mempty@.
-}
instance Semigroup (CompilerJournal a) where
  a <> b
    | isEmptyJournal a = b
    | isEmptyJournal b = a
    | otherwise =
        CompilerJournal
          { compilerJournalPatterns = compilerJournalPatterns a <> compilerJournalPatterns b
          , compilerJournalRecordEntries = compilerJournalRecordEntries a <> compilerJournalRecordEntries b
          , compilerJournalDictionaryTraits = compilerJournalDictionaryTraits a <> compilerJournalDictionaryTraits b
          , compilerJournalErrors = compilerJournalErrors a <> compilerJournalErrors b
          }

-- | True when every journal field is empty, i.e. the value is @mempty@.
isEmptyJournal :: (CompilerJournal a) -> Bool
isEmptyJournal CompilerJournal{..} =
  null compilerJournalPatterns
    && null compilerJournalRecordEntries
    && Set.null compilerJournalDictionaryTraits
    && null compilerJournalErrors

instance Monoid (CompilerJournal a) where
  mempty = CompilerJournal mempty mempty mempty mempty

{-# INLINE tellPatterns #-}
tellPatterns :: (MonadWriter (CompilerJournal a) m) => [(Name, Pattern a Kind IndexedType)] -> m ()
tellPatterns w = tell $ CompilerJournal w mempty mempty mempty

{-# INLINE tellPatterns1 #-}
tellPatterns1 :: (MonadWriter (CompilerJournal a) m) => (Name, Pattern a Kind IndexedType) -> m ()
tellPatterns1 w = tellPatterns [w]

{-# INLINE tellRecordEntry #-}
tellRecordEntry :: (MonadWriter (CompilerJournal a) m) => [RecordEntry a] -> m ()
tellRecordEntry w = tell $ CompilerJournal mempty w mempty mempty

{-# INLINE tellDictionaryTraits #-}
tellDictionaryTraits :: (MonadWriter (CompilerJournal a) m) => Set (Trait IndexedType) -> m ()
tellDictionaryTraits w = tell $ CompilerJournal mempty mempty w mempty

{-# INLINE tellErrors #-}
tellErrors :: (MonadWriter (CompilerJournal a) m) => [CompilerError a] -> m ()
tellErrors w = tell $ CompilerJournal mempty mempty mempty w

listenPatterns :: (MonadWriter (CompilerJournal a) m) => m e -> m (e, [(Name, Pattern a Kind IndexedType)])
listenPatterns w = pass $ do
  (result, journal) <- listen w
  pure ((result, compilerJournalPatterns journal), \j -> j{compilerJournalPatterns = mempty})

listenRecordEntry :: (MonadWriter (CompilerJournal a) m) => m e -> m (e, [RecordEntry a])
listenRecordEntry w = second compilerJournalRecordEntries <$> listen w

listenDictionaryTraits :: (MonadWriter (CompilerJournal a) m) => m e -> m (e, Set (Trait IndexedType))
listenDictionaryTraits w = second compilerJournalDictionaryTraits <$> listen w

listenErrors :: (MonadWriter (CompilerJournal a) m) => m e -> m (e, [CompilerError a])
listenErrors w = second compilerJournalErrors <$> listen w

censorDictionaryTraits :: (MonadWriter (CompilerJournal a) m) => (Set (Trait IndexedType) -> Set (Trait IndexedType)) -> m b -> m b
censorDictionaryTraits f =
  censor $
    \CompilerJournal{..} ->
      CompilerJournal
        { compilerJournalDictionaryTraits =
            f compilerJournalDictionaryTraits
        , ..
        }
