{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Type (
  Type (..),
  TypeIndex (..),
  Parameter (..),
  HasActive (..),
  IndexedType,
  foldType,
  unfoldType,
  activeIdsIn,
  normalizeRowTypes,
  listType,
  tupleType,
  (~>),
) where

import Coal.Common.Supply (Supply (..))
import Coal.Language.Type.Intrinsic (Intrinsic (..))
import Coal.Language.Type.Kind (Kind (..), tupleKind)
import Coal.Language.Type.Row (Row (..), normalizeRow)
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (transform)
import Data.List.NonEmpty (NonEmpty (..), toList, (<|))
import Data.Text (isPrefixOf)
import Extra (Map, Name, Set)
import Extra.Prettyprinter (parensIf)
import GHC.Generics (Generic)
import Prettyprinter
import TextShow (showt)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set

data Type o k
  = TApplication k (Type o k) (NonEmpty (Type o k))
  | TArrow (Type o k) (Type o k)
  | TConstructor k Name
  | TIntrinsic (Intrinsic (Type o k))
  | TRow (Row o k (Type o k))
  | TVariable (o k)
  | TAlias Name [Type o k] (Type o k)
  deriving (Show, Eq, Ord, Read, Data, Typeable, Generic)

infixr 1 `TArrow`

(~>) :: Type o k -> Type o k -> Type o k
(~>) = TArrow

infixr 1 ~>

data TypeIndex k = TypeIndex
  { typeIndexKind :: k
  , typeIndexId :: Int
  }
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Data, Typeable, Generic)

instance Pretty (TypeIndex k) where
  pretty (TypeIndex _ i) = pretty i

data Parameter k = Parameter
  { parameterKind :: k
  , parameterName :: Name
  }
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Data, Typeable, Generic)

instance Pretty (Parameter k) where
  pretty (Parameter _ name) = pretty name

type IndexedType = Type TypeIndex Kind

instance Supply (TypeIndex k) where
  updateSupply f (TypeIndex k t) = TypeIndex k (f t)
  getSupply (TypeIndex _ t) = t

class HasActive k t | t -> k where
  activeIn :: t -> Set (TypeIndex k)

instance (HasActive Kind t) => HasActive Kind (Map a t) where
  activeIn = Set.unions . fmap activeIn

instance (HasActive Kind t) => HasActive Kind [t] where
  activeIn = Set.unions . fmap activeIn

instance (HasActive Kind t) => HasActive Kind (NonEmpty t) where
  activeIn = Set.unions . fmap activeIn

{-# INLINE activeIdsIn #-}
activeIdsIn :: (HasActive k t) => t -> Set Int
activeIdsIn = Set.map typeIndexId . activeIn

{-# INLINE foldType #-}
foldType :: (Foldable f) => Type o k -> f (Type o k) -> Type o k
foldType = foldr TArrow

unfoldType :: Type o k -> NonEmpty (Type o k)
unfoldType =
  \case
    TArrow t1 t2 ->
      t1 <| unfoldType t2
    t ->
      NonEmpty.singleton t

normalizeRowTypes :: (Typeable o, Data k, Data (o k)) => Type o k -> Type o k
normalizeRowTypes = transform $
  \case
    TRow r ->
      TRow (normalizeRow r)
    t ->
      t

listType :: IndexedType -> IndexedType
listType t = TApplication KType (TConstructor (KArrow KType KType) "List") (t :| [])

tupleType :: NonEmpty IndexedType -> IndexedType
tupleType ts = TApplication KType (TConstructor (tupleKind (length ts)) cons) ts
 where
  cons = "#Tuple" <> showt (length ts)

precArrow, precApp, precAtom :: Int
precArrow = 1 -- e.g., a -> b
precApp = 2 -- e.g., T(x, y)
precAtom = 3 -- variables, constructors, literals

typeBrackets :: [Doc ann] -> Doc ann
typeBrackets = encloseSep "<" ">" ", "

instance (Pretty k, Pretty (o k)) => Pretty (Type o k) where
  pretty = prettyTypePrec 0

prettyTypePrec :: (Pretty k, Pretty (o k)) => Int -> Type o k -> Doc ann
prettyTypePrec prec =
  \case
    TArrow t1 t2 ->
      parensIf (prec > precArrow) $
        group (prettyTypePrec (precArrow + 1) t1 <+> "→" <+> prettyTypePrec precArrow t2)
    TApplication _ (TConstructor _ con) args
      | "#Tuple" `isPrefixOf` con ->
          parensIf (prec > precApp) $ group (tupled (map (prettyTypePrec 0) (toList args)))
    TApplication _ f args ->
      parensIf (prec > precApp) $
        group (prettyTypePrec precApp f <> typeBrackets (map (prettyTypePrec 0) (toList args)))
    TConstructor _ name ->
      pretty name
    TVariable v ->
      pretty v
    TIntrinsic i ->
      prettyIntrinsic (prettyTypePrec precAtom) i
    TRow row ->
      prettyRow (prettyTypePrec precAtom) row
    TAlias name args t ->
      parensIf (prec > precApp) $
        group $
          "alias"
            <+> (pretty name <> prettyArgs)
            <+> "="
            <+> prettyTypePrec precArrow t
     where
      prettyArgs
        | null args = ""
        | otherwise = typeBrackets (map (prettyTypePrec 0) args)

prettyIntrinsic :: (t -> Doc ann) -> Intrinsic t -> Doc ann
prettyIntrinsic prettyT =
  \case
    IBool ->
      "bool"
    IChar ->
      "char"
    IDouble ->
      "double"
    IFloat ->
      "float"
    IInt32 ->
      "int32"
    IInt64 ->
      "int64"
    IBignum ->
      "bignum"
    INat ->
      "nat"
    IString ->
      "string"
    IUnit ->
      "unit"
    IVoid ->
      "void"
    IRecord t ->
      braces $ enclose space space (prettyT t)

prettyRow :: (Pretty (o k)) => (t -> Doc ann) -> Row o k t -> Doc ann
prettyRow prettyT = fields
 where
  fields =
    \case
      RExtend name ty rest ->
        pretty name <+> ":" <+> prettyT ty <> fieldSep rest
       where
        fieldSep RNil = mempty
        fieldSep RVariable{} = " |" <+> fields rest
        fieldSep _ = "," <+> fields rest
      RVariable v ->
        pretty v
      RNil ->
        "{}"
