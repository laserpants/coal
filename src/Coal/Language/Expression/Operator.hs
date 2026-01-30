{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Expression.Operator (
  Operator (..),
  operatorTypeScheme,
) where

import Coal.Language.Type (Type (..), listType, (~>))
import Coal.Language.Type.Intrinsic (Intrinsic (..))
import Coal.Language.Type.Scheme (IndexedScheme, Scheme (..), forall0, forall1, forall2, forall3)
import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

-- | Operators
data Operator
  = -- | Logical NOT (!)
    OLogicalNot
  | -- | Logical OR (||)
    OLogicalOr
  | -- | Logical AND (&&)
    OLogicalAnd
  | -- | Forward application (.|)
    OForwardApplication
  | -- | Reverse application (|.)
    OReverseApplication
  | -- | Forward composition (>>)
    OForwardComposition
  | -- | Reverse composition (<<)
    OReverseComposition
  | -- | String concatenation (+++)
    OStringConcatenation
  | -- | List concatenation (++)
    OListConcatenation
  deriving (Show, Eq, Ord, Read, Data, Typeable, Generic)

instance Binary Operator

operatorTypeScheme :: Operator -> IndexedScheme
operatorTypeScheme =
  \case
    OLogicalNot ->
      forall0 (TIntrinsic IBool ~> TIntrinsic IBool)
    OReverseApplication ->
      forall2 (\a b -> a ~> (a ~> b) ~> b)
    OForwardApplication ->
      forall2 (\a b -> (a ~> b) ~> a ~> b)
    OReverseComposition ->
      forall3 (\a b c -> (b ~> c) ~> (a ~> b) ~> a ~> c)
    OForwardComposition ->
      forall3 (\a b c -> (a ~> b) ~> (b ~> c) ~> a ~> c)
    OLogicalOr ->
      forall0 (TIntrinsic IBool ~> TIntrinsic IBool ~> TIntrinsic IBool)
    OLogicalAnd ->
      forall0 (TIntrinsic IBool ~> TIntrinsic IBool ~> TIntrinsic IBool)
    OListConcatenation ->
      forall1 (\a -> listType a ~> listType a ~> listType a)
    OStringConcatenation ->
      Forall mempty [] (TIntrinsic IString ~> TIntrinsic IString ~> TIntrinsic IString)
