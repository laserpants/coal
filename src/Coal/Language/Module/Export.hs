{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Language.Module.Export

Module export declarations for names and types.

An export atom mirrors the syntax used for imports:

@
T(A, B)  -- type @T@ together with constructors @A@ and @B@
T()      -- type @T@ with none of its constructors
T        -- type @T@ with all of its constructors
@

The last form is represented as the wildcard member list @["*"]@, exactly
like a wildcard import.  The wildcard is resolved to the concrete member set
during build preparation
('Coal.Compiler.Pass.PhaseTypeChecking.PrepareBuild.expandExports'), so
'exportsName' only ever observes explicit member lists.
-}
module Coal.Language.Module.Export (
  Export (..),
  includesMember,
  includesName,
  exportsMember,
  exportsName,
) where

import Data.Data (Data, Typeable)
import Extras (Name)

data Export a
  = NameExport a Name
  | TypeExport a Name [Name]
  deriving (Show, Eq, Ord, Read, Data, Typeable)

{- | Whether a single export atom includes @name@.

For a 'TypeExport' the exported name itself (a type, type alias or trait) is
always included, as is any name appearing in the member list.
-}
exportsName :: Name -> Export a -> Bool
exportsName name = \case
  NameExport _ exportName ->
    exportName == name
  TypeExport _ exportName members ->
    exportName == name || name `elem` members

includesName :: [Export a] -> Name -> Bool
includesName exports = flip any exports . exportsName

{- | Whether @member@ — a data constructor or trait interface member belonging to
@owner@ — is selected by a single export atom.

Unlike 'exportsName', a member is selected only when it appears in the member
list of the 'TypeExport' for @owner@.  This distinction matters when a type and
one of its constructors share a name (@type Thing = Thing(int32)@): exporting
the type with an empty member list (@Thing()@) must export the type but not the
same-named constructor.
-}
exportsMember :: Name -> Name -> Export a -> Bool
exportsMember owner member = \case
  NameExport _ exportName ->
    exportName == member
  TypeExport _ exportName members ->
    exportName == owner && member `elem` members

-- | Whether any export atom in the list selects @member@ as a member of @owner@.
includesMember :: [Export a] -> Name -> Name -> Bool
includesMember exports owner member = flip any exports (exportsMember owner member)
