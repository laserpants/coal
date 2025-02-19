module Noll.Common.List1 (
  module Data.List.NonEmpty,
  List1,
  fromList1,
  concat,
) where

import Data.List.NonEmpty (
  NonEmpty (..),
  partition,
  reverse,
  length,
  singleton,
  unzip,
  (<|),
 )

import qualified Data.List.NonEmpty as NonEmpty

type List1 = NonEmpty

{-# INLINE fromList1 #-}
fromList1 :: List1 a -> [a]
fromList1 = NonEmpty.toList
