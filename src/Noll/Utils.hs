module Noll.Utils where

import Data.Char (ord)
import Data.Hashable (Hashable, hash)
import Data.Text (Text)
import Numeric (showHex)

import qualified Data.Text as Text

lexOrderRank :: Text -> Int
lexOrderRank text
  | Text.null text =
      error "Empty string"
  | otherwise =
      snd (Text.foldr f (0, 0) text) - 1
 where
  f :: Char -> (Int, Int) -> (Int, Int)
  f c (m, n) = (m + 1, n + (36 ^ m) + g (ord c))
  g n
    | not ((n >= 97 && n <= 122) || (n >= 48 && n <= 57)) =
        error "Invalid character"
    | n >= 97 =
        n - 97
    | otherwise =
        n - 22

hashed :: (Hashable a) => a -> Text
hashed t = Text.pack (showHex (fromIntegral (hash t) :: Word) "")
