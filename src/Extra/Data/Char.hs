module Extra.Data.Char (isUnderscore) where

{-# INLINE isUnderscore #-}
isUnderscore :: Char -> Bool
isUnderscore = ('_' ==)
