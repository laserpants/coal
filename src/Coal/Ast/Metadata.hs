{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Coal.Ast.Metadata (Metadata (..)) where

import Data.Data (Data)
import Text.Megaparsec

data Metadata = Metadata
    { locationStart :: SourcePos
    , locationEnd :: SourcePos
    }
  deriving (Show, Eq, Ord, Read, Data)
  
defaultSourcePos :: SourcePos
defaultSourcePos = SourcePos
  { sourceName = "<unknown>"
  , sourceLine = mkPos 1
  , sourceColumn = mkPos 1
  }

instance Semigroup Metadata where
  lhs <> _ = lhs

instance Monoid Metadata where
  mempty = Metadata defaultSourcePos defaultSourcePos
