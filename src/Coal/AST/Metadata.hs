{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Coal.AST.Metadata (Metadata (..)) where

import Data.Binary (Binary (..))
import Data.Data (Data)
import GHC.Generics (Generic)
import Text.Megaparsec (SourcePos (..), mkPos)

data Metadata = Metadata
  { locationStart :: SourcePos
  , locationEnd :: SourcePos
  }
  deriving (Eq, Ord, Read, Data, Generic)

-- TODO: remove
instance Show Metadata where
  show _ = ""

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
