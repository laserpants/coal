{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Type.Kind (
  Kind (..),
  KindProxy (..),
  foldKind,
  unfoldKind,
  applyKind,
  tupleKind,
  tupleConstructorKind,
)
where

import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Extras (Name)
import GHC.Generics (Generic)

data Kind
  = KType
  | KRow
  | KArrow Kind Kind
  | KTrait
  | KVariable Int
  deriving (Show, Eq, Ord, Read, Data, Typeable, Generic)

instance Binary Kind

infixr 1 `KArrow`

{-# INLINE foldKind #-}
foldKind :: (Foldable f) => Kind -> f Kind -> Kind
foldKind = foldr KArrow

unfoldKind :: Kind -> NonEmpty Kind
unfoldKind =
  \case
    KArrow k1 k2 ->
      k1 <| unfoldKind k2
    k ->
      NonEmpty.singleton k

applyKind :: [Kind] -> Kind -> Maybe Kind
applyKind ks k
  | ks `isPrefixOf` ls && length ls > length ks =
      Just (foldr1 KArrow (drop (length ks) ls))
  | otherwise =
      Nothing
 where
  ls = NonEmpty.toList (unfoldKind k)

{-# INLINE tupleKind #-}
tupleKind :: Int -> Kind
tupleKind n = foldKind KType (replicate n KType)

tupleConstructorKind :: Name -> Kind
tupleConstructorKind con = tupleKind (read (drop 6 (Text.unpack con)))

class KindProxy t k where
  tailKind :: t -> k

instance (Data t) => KindProxy t Kind where
  tailKind t =
    case head (universeBi t) of
      KArrow _ k ->
        k
      _ ->
        error "Invalid kind"

instance KindProxy a () where
  tailKind _ = ()
