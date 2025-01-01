module Noll.Library.List1 (
  module Data.List.NonEmpty,
  List1,
  list1ToList,
) where

import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NonEmpty

type List1 = NonEmpty

list1ToList :: List1 a -> [a]
list1ToList = NonEmpty.toList
