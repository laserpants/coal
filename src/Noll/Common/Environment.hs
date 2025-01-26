{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Noll.Common.Environment (
  Environment (..),
  new,
  insert,
  insertMultiple,
  fromList,
  lookup,
  elems,
)
where

import Noll.Utils (Dictionary, Name)
import Prelude hiding (lookup)

import qualified Data.Map.Strict as Map

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

{-# INLINE insertMultiple #-}
insertMultiple :: (Foldable f) => f (Name, a) -> Environment a -> Environment a
insertMultiple = flip (foldr (uncurry insert))

{-# INLINE fromList #-}
fromList :: [(Name, a)] -> Environment a
fromList = (`insertMultiple` new)

{-# INLINE lookup #-}
lookup :: Name -> Environment a -> Maybe a
lookup name = Map.lookup name . environmentDictionary

{-# INLINE elems #-}
elems :: Environment a -> [a]
elems = Map.elems . environmentDictionary
