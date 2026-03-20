{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coal.Common.Environment (
  Environment (..),
  mapEnvironment,
  mapMEnvironment,
  forMEnvironment,
  new,
  insert,
  insertWith,
  insertMultiple,
  fromList,
  toList,
  lookup,
  lookupWithDefault,
  contains,
  restrict,
  lookupAll,
  elems,
  filter,
  filterNames,
  names,
  union,
  adjust,
  alter,
) where

import Data.Binary (Binary)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Extras (Dictionary, Name, Over)
import GHC.Generics (Generic)
import Prelude hiding (filter, lookup)

newtype Environment e = Environment {envDictionary :: Dictionary e}
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Semigroup
    , Monoid
    , Generic
    )

instance (Binary e) => Binary (Environment e)

{-# INLINE overEnvironment #-}
overEnvironment :: Over (Environment e) (Dictionary e)
overEnvironment fn (Environment e) = Environment (fn e)

{-# INLINE mapEnvironment #-}
mapEnvironment :: (a -> b) -> Environment a -> Environment b
mapEnvironment f (Environment e) = Environment (fmap f e)

{-# INLINE mapMEnvironment #-}
mapMEnvironment :: (Monad m) => (a -> m b) -> Environment a -> m (Environment b)
mapMEnvironment f (Environment e) = Environment <$> traverse f e

{-# INLINE forMEnvironment #-}
forMEnvironment :: (Monad m) => Environment a -> (a -> m b) -> m (Environment b)
forMEnvironment = flip mapMEnvironment

{-# INLINE new #-}
new :: Environment a
new = mempty

{-# INLINE insert #-}
insert :: Name -> a -> Environment a -> Environment a
insert name = overEnvironment . Map.insert name

{-# INLINE adjust #-}
adjust :: (a -> a) -> Name -> Environment a -> Environment a
adjust f name = overEnvironment (Map.adjust f name)

{-# INLINE alter #-}
alter :: (Maybe a -> Maybe a) -> Name -> Environment a -> Environment a
alter f name = overEnvironment (Map.alter f name)

{-# INLINE insertWith #-}
insertWith :: (a -> a -> a) -> Name -> a -> Environment a -> Environment a
insertWith f name = overEnvironment . Map.insertWith f name

insertMultiple :: (Foldable f) => f (Name, a) -> Environment a -> Environment a
insertMultiple = flip (foldr (uncurry insert))

{-# INLINE fromList #-}
fromList :: [(Name, a)] -> Environment a
fromList = (`insertMultiple` new)

{-# INLINE toList #-}
toList :: Environment a -> [(Name, a)]
toList = Map.toList . envDictionary

{-# INLINE lookup #-}
lookup :: Name -> Environment a -> Maybe a
lookup name = Map.lookup name . envDictionary

{-# INLINE lookupWithDefault #-}
lookupWithDefault :: a -> Name -> Environment a -> a
lookupWithDefault value name = fromMaybe value . Map.lookup name . envDictionary

contains :: Name -> Environment a -> Bool
contains name (Environment e) = Map.member name e

lookupAll :: [Name] -> Environment a -> [(Name, a)]
lookupAll names_ env = Map.toList e where Environment e = restrict names_ env

{-# INLINE filter #-}
filter :: (a -> Bool) -> Environment a -> Environment a
filter f (Environment e) = Environment (Map.filter f e)

filterNames :: (Name -> Bool) -> Environment a -> Environment a
filterNames f (Environment e) = Environment (Map.filterWithKey (const . f) e)

restrict :: [Name] -> Environment a -> Environment a
restrict names_ = filterNames (`elem` names_)

{-# INLINE elems #-}
elems :: Environment a -> [a]
elems = Map.elems . envDictionary

{-# INLINE names #-}
names :: Environment a -> [Name]
names = Map.keys . envDictionary

union :: Environment a -> Environment a -> Environment a
union (Environment e1) (Environment e2) = Environment (e1 `Map.union` e2)
