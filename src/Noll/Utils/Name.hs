module Noll.Utils.Name (
  Name,
  Dictionary,
  isConstructor,
) where

import Data.Char (isAlpha, isUpper, ord)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Noll.Utils.Operators ((||.))

import qualified Data.Text as Text

type Name = Text

type Dictionary = Map Name

{-# INLINE dropWhileNot #-}
dropWhileNot :: (Char -> Bool) -> Text -> Text
dropWhileNot = Text.dropWhile . fmap not

isConstructor :: Name -> Bool
isConstructor name
  | Text.null name = error "Empty name"
  | Text.null s = False
  | otherwise = isUpper (Text.head s)
 where
  s = dropWhileNot (isAlpha ||. ('_' ==)) name
