{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Export (Export (..), includesName) where

import Coal.Common.Name (isConstructor)
import Data.Data (Data, Typeable)
import Extras (Name)

data Export a
  = NameExport a Name
  | TypeExport a Name [Name]
  deriving (Show, Eq, Ord, Read, Data, Typeable)

exportsName :: Name -> Export a -> Bool
exportsName name export
  | isConstructor name =
      case export of
        TypeExport _ exportName _ ->
          exportName == name
        NameExport{} ->
          False
  | otherwise =
      case export of
        NameExport _ exportName ->
          exportName == name
        TypeExport _ _ names ->
          name `elem` names

includesName :: [Export a] -> Name -> Bool
includesName exports = flip any exports . exportsName
