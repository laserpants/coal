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
  KindProxy (..),
  IndexedType,
  ParameterizedType,
  foldType,
  unfoldType,
  activeIdsIn,
  normalizeRowTypes,
  applyTypeArgs,
  listTypeArgs,
  listType,
  tupleType,
  tupleTypeConstructor,
  isTupleType,
  fieldsRecordType,
  recordType,
  headConstructor,
  constructors,
  (~>),
) where

import Coal.Common.Supply (Supply (..))
import Coal.Language.Type.Intrinsic (Intrinsic (..))
import Coal.Language.Type.Kind (Kind (..), tupleKind)
import Coal.Language.Type.Row (Row (..), fromDictionary, normalizeRow)
import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (transform, universeBi)
import Data.List.NonEmpty (NonEmpty (..), toList, (<|))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Data.Text (isPrefixOf)
import Extras (Dictionary, Map, Name, Set)
import Extras.Data.Set (unionMap)
import Extras.Prettyprinter (parensIf)
import GHC.Generics (Generic)
import Prettyprinter
import TextShow (showt)

data Type o k
  = TApplication k (Type o k) (Type o k)
  | TArrow (Type o k) (Type o k)
  | TConstructor k Name
  | TIntrinsic Intrinsic
  | TRecord (Type o k)
  | TRow (Row o k (Type o k))
  | TVariable (o k)
  | TAlias Name [Type o k] (Type o k)
  deriving (Show, Eq, Ord, Read, Data, Typeable, Generic)

instance (Binary (o k), Binary k) => Binary (Type o k)

infixr 1 `TArrow`

(~>) :: Type o k -> Type o k -> Type o k
(~>) = TArrow

infixr 1 ~>

data TypeIndex k = TypeIndex
  { typeIndexKind :: k
  , typeIndexId :: Int
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Data
    , Typeable
    , Generic
    )

instance (Binary k) => Binary (TypeIndex k)

instance Pretty (TypeIndex k) where
  pretty (TypeIndex _ i) = "t" <> "." <> pretty i

data Parameter k = Parameter
  { parameterKind :: k
  , parameterName :: Name
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Data
    , Typeable
    , Generic
    )

instance Pretty (Parameter k) where
  pretty (Parameter _ name) = pretty name

instance (Binary k) => Binary (Parameter k)

type IndexedType = Type TypeIndex Kind

type ParameterizedType = Type Parameter ()

instance Supply (TypeIndex k) where
  updateSupply f (TypeIndex k t) = TypeIndex k (f t)
  getSupply (TypeIndex _ t) = t

class HasActive k t | t -> k where
  activeIn :: t -> Set (TypeIndex k)

instance (HasActive Kind t) => HasActive Kind (Map a t) where
  activeIn = unionMap activeIn

instance (HasActive Kind t) => HasActive Kind [t] where
  activeIn = unionMap activeIn

instance (HasActive Kind t) => HasActive Kind (NonEmpty t) where
  activeIn = unionMap activeIn

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

class KindProxy o k where
  tailKind :: Type o k -> k

instance KindProxy TypeIndex Kind where
  tailKind t =
    case head (universeBi t) of
      KArrow _ k ->
        k
      _ ->
        error "Invalid kind"

instance KindProxy a () where
  tailKind _ = ()

applyTypeArgs :: (KindProxy o k) => k -> Type o k -> NonEmpty (Type o k) -> Type o k
applyTypeArgs k ty = go ty . NonEmpty.toList
 where
  go t =
    \case
      [t1] ->
        TApplication k t t1
      t1 : ts ->
        go (TApplication (tailKind t) t t1) ts
      _ ->
        error "Implementation error"

listTypeArgs :: Type o k -> (Type o k, NonEmpty (Type o k))
listTypeArgs (TApplication _ t1 t2) = (t, NonEmpty.prependList ts (NonEmpty.singleton t2))
 where
  (t, ts) = go t1
  go =
    \case
      TApplication _ u1 u2 ->
        let (u, us) = go u1 in (u, us <> [u2])
      u ->
        (u, [])
listTypeArgs _ = error "Implementation error"

headConstructor :: Type o k -> Maybe Name
headConstructor =
  \case
    TApplication _ t _ ->
      headConstructor t
    TConstructor _ name ->
      Just name
    _ ->
      Nothing

listType :: IndexedType -> IndexedType
listType t = applyTypeArgs KType (TConstructor (KArrow KType KType) "List") (t :| [])

tupleType :: NonEmpty IndexedType -> IndexedType
tupleType ts = applyTypeArgs KType (TConstructor (tupleKind n) (tupleTypeConstructor n)) ts
 where
  n = length ts

isTupleType :: Type o k -> Bool
isTupleType t =
  case headConstructor t of
    Just con
      | "#Tuple" `isPrefixOf` con ->
          True
    _ ->
      False

{-# INLINE tupleTypeConstructor #-}
tupleTypeConstructor :: Int -> Name
tupleTypeConstructor n = "#Tuple" <> showt n

{-# INLINE recordType #-}
recordType :: Row o k (Type o k) -> Type o k
recordType = TRecord . TRow

fieldsRecordType :: Dictionary (Type o k) -> Row o k (Type o k) -> Type o k
fieldsRecordType fields row = recordType (fromDictionary fields row)

constructors :: Type o k -> Set Name
constructors =
  \case
    TApplication _ t1 t2 ->
      constructors t1 <> constructors t2
    TArrow t1 t2 ->
      constructors t1 <> constructors t2
    TConstructor _ name ->
      Set.singleton name
    TIntrinsic{} ->
      mempty
    TRecord r ->
      constructors r
    TRow r ->
      constructorsRow r
    TVariable{} ->
      mempty
    TAlias name _ t ->
      Set.insert name (constructors t)

constructorsRow :: Row o k (Type o k) -> Set Name
constructorsRow =
  \case
    RExtend _ t r ->
      constructors t <> constructorsRow r
    RVariable{} ->
      mempty
    RNil ->
      mempty

precArrow, precApp, precAtom :: Int
precArrow = 1 -- e.g., a -> b
precApp = 2 -- e.g., T(x, y)
precAtom = 3 -- variables, constructors, literals

typeBrackets :: [Doc ann] -> Doc ann
typeBrackets = encloseSep "<" ">" ", "

instance (Show (o k), Show k, Pretty k, Pretty (o k)) => Pretty (Type o k) where
  pretty = prettyTypePrec 0

prettyTypeApplicationPrec :: (Show (o k), Show k, Pretty k, Pretty (o k)) => Int -> Type o k -> NonEmpty (Type o k) -> Doc ann
prettyTypeApplicationPrec prec con args
  | isTupleType con =
      parensIf (prec > precApp) $ group (tupled (map (prettyTypePrec 0) (toList args)))
prettyTypeApplicationPrec prec con args =
  parensIf (prec > precApp) $
    group (prettyTypePrec precApp con <> typeBrackets (map (prettyTypePrec 0) (toList args)))

prettyTypePrec :: (Show (o k), Show k, Pretty k, Pretty (o k)) => Int -> Type o k -> Doc ann
prettyTypePrec prec =
  \case
    TArrow t1 t2 ->
      parensIf (prec > precArrow) $
        group (prettyTypePrec (precArrow + 1) t1 <+> "->" <+> prettyTypePrec precArrow t2)
    t@TApplication{} ->
      uncurry (prettyTypeApplicationPrec prec) (listTypeArgs t)
    TConstructor _ name ->
      pretty name
    TVariable v ->
      pretty v
    TIntrinsic i ->
      prettyIntrinsic i
    TRecord t ->
      braces $ enclose space space (prettyTypePrec precAtom t)
    TRow row ->
      prettyRow (prettyTypePrec precAtom) row
    TAlias name args t ->
      parensIf (prec > precApp) $
        group $
          "type alias"
            <+> (pretty name <> prettyArgs)
            <+> "="
            <+> prettyTypePrec precArrow t
     where
      prettyArgs
        | null args = ""
        | otherwise = typeBrackets (map (prettyTypePrec 0) args)

prettyIntrinsic :: Intrinsic -> Doc ann
prettyIntrinsic =
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
