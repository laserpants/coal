module Noll.Library.Environment where

newtype Environment e = Environment {environmentDictionary :: Dictionary e}
  deriving (Show, Eq, Ord, Read, Semigroup, Monoid)
