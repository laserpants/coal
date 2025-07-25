{-# LANGUAGE OverloadedStrings #-}

module Coal.Utils (lexOrderRank) where

import Data.Char (ord)
import Data.Text (Text)

import qualified Data.Text as Text

{-# INLINE inCharRange #-}
inCharRange :: Int -> (Char, Char) -> Bool
inCharRange n (a, b) = n >= ord a && n <= ord b

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
    | not (n `inCharRange` ('a', 'z') || n `inCharRange` ('0', '9')) =
        error "Invalid character"
    | n >= ord 'a' = n - ord 'a'
    | otherwise = n - 22
