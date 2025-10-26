module Extra.Text.Megaparsec.Char (singleQuote, doubleQuote) where

import Data.Text (Text)
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Char (char)

{-# INLINE singleQuote #-}
singleQuote :: (Ord a) => Parsec a Text Char
singleQuote = char '\''

{-# INLINE doubleQuote #-}
doubleQuote :: (Ord a) => Parsec a Text Char
doubleQuote = char '"'
