{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Language.Type.Operations (
  -- * Smart constructors
  listType,
  tupleType,
  tupleTypeConstructor,
  recordType,
  fieldsRecordType,

  -- * Query functions
  headConstructor,
  constructors,
  isTupleType,
  typeArgs,

  -- * Transformations
  unfoldType,
  rowNormalize,
)
where

import Coal.Language.Type (IndexedType, Type (..), applyTypeArgs)
import Coal.Language.Type.Kind (Kind (..), tupleKind)
import Coal.Language.Type.Row (Row (..), fromDictionary, normalizeRow)
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (transform)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Data.Text (isPrefixOf)
import Extras (Dictionary, Name, Set)
import TextShow (showt)

-- | Construct a list type from an element type
listType :: IndexedType -> IndexedType
listType t = applyTypeArgs KType (TConstructor (KArrow KType KType) "List") (t :| [])

-- | Construct a tuple type from a non-empty list of element types
tupleType :: NonEmpty IndexedType -> IndexedType
tupleType ts = applyTypeArgs KType (TConstructor (tupleKind n) (tupleTypeConstructor n)) ts
 where
  n = length ts

-- | Get the name of the tuple type constructor for a given arity
{-# INLINE tupleTypeConstructor #-}
tupleTypeConstructor :: Int -> Name
tupleTypeConstructor n = "#Tuple" <> showt n

-- | Construct a record type from a row
{-# INLINE recordType #-}
recordType :: Row o k (Type o k) -> Type o k
recordType = TRecord . TRow

-- | Construct a record type from a dictionary of fields and a row variable
fieldsRecordType :: Dictionary (Type o k) -> Row o k (Type o k) -> Type o k
fieldsRecordType fields row = recordType (fromDictionary fields row)

-- | Extract the head constructor name from a type (if it's an application or constructor)
headConstructor :: Type o k -> Maybe Name
headConstructor =
  \case
    TApplication _ t _ ->
      headConstructor t
    TConstructor _ name ->
      Just name
    _ ->
      Nothing

-- | Check if a type is a tuple type
isTupleType :: Type o k -> Bool
isTupleType t =
  case headConstructor t of
    Just con
      | "#Tuple" `isPrefixOf` con ->
          True
    _ ->
      False

-- | Decompose a type application into its head and arguments
typeArgs :: Type o k -> (Type o k, NonEmpty (Type o k))
typeArgs (TApplication _ t1 t2) = (t, NonEmpty.prependList ts (NonEmpty.singleton t2))
 where
  (t, ts) = go t1
  go =
    \case
      TApplication _ u1 u2 ->
        let (u, us) = go u1 in (u, us <> [u2])
      u ->
        (u, [])
typeArgs _ = error "Implementation error"

-- | Unfold a function type into a non-empty list of types (inverse of foldType)
unfoldType :: Type o k -> NonEmpty (Type o k)
unfoldType =
  \case
    TArrow t1 t2 ->
      t1 <| unfoldType t2
    t ->
      NonEmpty.singleton t

-- | Normalize rows in a type by applying row normalization
rowNormalize :: (Typeable o, Data k, Data (o k)) => Type o k -> Type o k
rowNormalize = transform $
  \case
    TRow r ->
      TRow (normalizeRow r)
    t ->
      t

-- | Collect all constructor names appearing in a type
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
      rowConstructors r
    TVariable{} ->
      mempty
    TAlias name _ t ->
      Set.insert name (constructors t)

rowConstructors :: Row o k (Type o k) -> Set Name
rowConstructors =
  \case
    RExtend _ t r ->
      constructors t <> rowConstructors r
    RVariable{} ->
      mempty
    RNil ->
      mempty
