module Noll.Common.List1 (
  module Data.List.NonEmpty,
  List1,
  fromList1,
) where

import Data.List.NonEmpty (NonEmpty (..), reverse, (<|))

import qualified Data.List.NonEmpty as NonEmpty

type List1 = NonEmpty

fromList1 :: List1 a -> [a]
fromList1 = NonEmpty.toList
