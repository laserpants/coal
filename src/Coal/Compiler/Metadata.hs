{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Metadata (Metadata (..), isDefaultMetadata) where

import Data.Binary (Binary (..))
import Data.Data (Data)
import GHC.Generics (Generic)
import Text.Megaparsec (SourcePos (..), mkPos)

data Metadata = Metadata
  { locationStart :: SourcePos
  , locationEnd :: SourcePos
  }
  deriving (Show, Eq, Ord, Read, Data, Generic)

instance Binary Metadata where
  put _ = pure ()
  get = pure mempty

defaultSourcePos :: SourcePos
defaultSourcePos =
  SourcePos
    { sourceName = "<unknown>"
    , sourceLine = mkPos 1
    , sourceColumn = mkPos 1
    }

instance Semigroup Metadata where
  lhs <> _ = lhs

instance Monoid Metadata where
  mempty = Metadata defaultSourcePos defaultSourcePos

{- | True when a node carries no real source location, i.e. it still holds the
default ('mempty') position produced by passes that synthesize AST nodes.
Callers use this to avoid rendering a misleading @1:1@ snippet for such nodes.
-}
isDefaultMetadata :: Metadata -> Bool
isDefaultMetadata m =
  locationStart m == defaultSourcePos && locationEnd m == defaultSourcePos
