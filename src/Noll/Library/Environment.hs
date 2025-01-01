{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Noll.Library.Environment (
  Environment (..),
  new,
  insert,
  insertMany,
  fromList,
  lookup,
  elems,
)
where

import qualified Data.Map.Strict as Map
import Noll.Utils (Dictionary, Name)
import Prelude hiding (lookup)

newtype Environment e = Environment {environmentDictionary :: Dictionary e}
  deriving (Show, Eq, Ord, Read, Semigroup, Monoid)

{-# INLINE overEnvironment #-}
overEnvironment :: (Dictionary e -> Dictionary e) -> Environment e -> Environment e
overEnvironment fn Environment{..} = Environment{environmentDictionary = fn environmentDictionary, ..}

{-# INLINE new #-}
new :: Environment a
new = mempty

{-# INLINE insert #-}
insert :: Name -> a -> Environment a -> Environment a
insert name val = overEnvironment (Map.insert name val)

{-# INLINE insertMany #-}
insertMany :: (Foldable f) => f (Name, a) -> Environment a -> Environment a
insertMany = flip (foldr (uncurry insert))

{-# INLINE fromList #-}
fromList :: (Foldable f) => f (Name, a) -> Environment a
fromList = (`insertMany` new)

{-# INLINE lookup #-}
lookup :: Name -> Environment a -> Maybe a
lookup name = Map.lookup name . environmentDictionary

{-# INLINE elems #-}
elems :: Environment a -> [a]
elems = Map.elems . environmentDictionary
