{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Record update desugaring.

Expands the partial record update syntax

@
record{ field = value, … }
@

into a record pattern match over the base record:

@
match(record) {
  | { field = $update.n, … | $update.m } =>
      { field = value, … | $update.m }
}
@

The generated match is further desugared by
'Coal.Compiler.Pass.PhaseTranslation.ExpandRecordPatterns', which turns the
record pattern into a @$Record@ constructor pattern plus 'EFocus' operations.
A record pattern's tail is a row-restricted /view/ of the base record: at
runtime it still carries every field (lookups are by name and return the most
recent extension), so re-extending the updated fields shadows their previous
values while leaving all other fields reachable.
-}
module Coal.Compiler.Pass.PhaseTranslation.ExpandRecordUpdates (
  passExpandRecordUpdates,
) where

import Coal.Common.Label (Label (..))
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformBiM)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Extras (Name)

passExpandRecordUpdates :: (Monad m, Data a) => Pass a m (Module a Kind IndexedType) (Module a Kind IndexedType)
passExpandRecordUpdates = Pass{runPass = transformBiM expandRecordUpdate}

expandRecordUpdate :: (Monad m) => Expression a Kind IndexedType -> CompilerT a m (Expression a Kind IndexedType)
expandRecordUpdate =
  \case
    ERecordUpdate loc t base fields -> do
      fieldNames <- Map.traverseWithKey (\_ _ -> supplied (freshName "update")) fields
      tailName <- supplied (freshName "update")
      let row =
            case t of
              TRecord (TRow r) -> r
              _ -> error "Implementation error: record update on a non-record type"
          tailRow = foldr dropFieldFromRow row (Map.keys fields)
          tailType = TRecord (TRow tailRow)
          fieldPattern field =
            case extractField field row of
              Just (fieldType, _) ->
                PVariable loc (Label fieldType (fieldNames Map.! field))
              Nothing ->
                error "Implementation error: record update field missing from record type"
          recordPattern =
            PRecord
              loc
              t
              (Map.mapWithKey (\field _ -> fieldPattern field) fields)
              (Just (PVariable loc (Label tailType tailName)))
          body = ERecord loc t fields (Just (EVariable loc (Label tailType tailName)))
      pure (EMatch loc t base (EClause loc recordPattern (CPlain loc [] body :| []) :| []))
    e ->
      pure e

dropFieldFromRow :: Name -> Row o k t -> Row o k t
dropFieldFromRow field row = maybe row snd (extractField field row)
