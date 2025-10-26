module Extras.Data.Text (dropWhileNot) where

import Data.Text (Text)

import qualified Data.Text as Text

{-# INLINE dropWhileNot #-}
dropWhileNot :: (Char -> Bool) -> Text -> Text
dropWhileNot = Text.dropWhile . fmap not
