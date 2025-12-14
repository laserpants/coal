module Utils (trim) where

import Data.Char (isSpace)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse
