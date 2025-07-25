module Coal.Common.List1 (
  module Data.List.NonEmpty,
  List1,
  fromList1,
  concat,
) where

import Data.List.NonEmpty (
  NonEmpty (..),
  appendList,
  head,
  init,
  last,
  length,
  nub,
  partition,
  prependList,
  reverse,
  singleton,
  sort,
  sortBy,
  unzip,
  zip,
  zipWith,
  (<|),
 )

import qualified Data.List.NonEmpty as NonEmpty

type List1 = NonEmpty

{-# INLINE fromList1 #-}
fromList1 :: List1 a -> [a]
fromList1 = NonEmpty.toList
