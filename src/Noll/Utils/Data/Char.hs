module Noll.Utils.Data.Char (isUnderscore) where

{-# INLINE isUnderscore #-}
isUnderscore :: Char -> Bool
isUnderscore = ('_' ==)
