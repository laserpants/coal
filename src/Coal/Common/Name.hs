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
import Extra.Data.Char (isUnderscore)
import Extra.Data.Text (dropWhileNot)
import Extra.Operators ((||.))

type Name = Text

type Dictionary = Map Name

isConstructor :: Name -> Bool
isConstructor qualified =
  case reverse (Text.splitOn "." qualified) of
    p : _ -> nameIsCtor p
    [] -> False

nameIsCtor :: Name -> Bool
nameIsCtor name
  | Text.null name = error "Empty name"
  | Text.null s = False
  | otherwise = isUpper (Text.head s)
 where
  s = dropWhileNot (isAlpha ||. isUnderscore) name
