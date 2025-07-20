{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Type.Kind (Kind (..), foldKind, applyKind) where

import Data.Data (Data, Typeable)
import Data.List (isPrefixOf)
import GHC.Generics (Generic)
import Lang.Common.List1 (List1, fromList1, (<|))

import qualified Lang.Common.List1 as List1

data Kind
  = KType
  | KRow
  | KArrow Kind Kind
  | KTrait
  deriving (Show, Eq, Ord, Read, Data, Typeable, Generic)

infixr 1 `KArrow`

{-# INLINE foldKind #-}
foldKind :: (Foldable f) => Kind -> f Kind -> Kind
foldKind = foldr KArrow

unfoldKind :: Kind -> List1 Kind
unfoldKind =
  \case
    KArrow k1 k2 ->
      k1 <| unfoldKind k2
    k ->
      List1.singleton k

applyKind :: [Kind] -> Kind -> Maybe Kind
applyKind ks k
  | ks `isPrefixOf` ls && length ls > length ks =
      Just (foldr1 KArrow (drop (length ks) ls))
  | otherwise =
      Nothing
 where
  ls = fromList1 (unfoldKind k)
