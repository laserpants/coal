{-# LANGUAGE OverloadedStrings #-}

module Coal.Common.Name (
  Name,
  Dictionary,
  isConstructor,
) where

import Data.Char (isAlpha, isUpper)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import Extras.Data.Char (isUnderscore)
import Extras.Data.Text (dropWhileNot)
import Extras.Operators ((||.))

type Name = Text

type Dictionary = Map Name

isConstructor :: Name -> Bool
isConstructor qualified =
  case reverse (Text.splitOn "." qualified) of
    p : _ ->
      isFirstAlphaUpper p
    [] ->
      False
 where
  isFirstAlphaUpper name
    | Text.null name = error "Empty name"
    | Text.null s = False
    | otherwise = isUpper (Text.head s)
   where
    s = dropWhileNot (isAlpha ||. isUnderscore) name
