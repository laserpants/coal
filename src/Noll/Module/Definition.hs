{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Module.Definition (Definition (..), Path (..), definitionName) where

import Data.Data (Data, Typeable)
import Lang.Utils (Name)
import Noll.Language.Constructor (Constructor (..))
import Noll.Language.Expression (Expression (..))
import Noll.Language.Trait (Trait (..), With (..))
import Noll.Language.Type (Parameter, Type (..))
import Noll.Module.Constant (Constant (..))
import Noll.Module.Function (Function (..))

newtype Path = Path {pathComponents :: [Name]}
  deriving (Show, Eq, Ord, Read, Data, Typeable)

data Definition a k t
  = -- | Type-annotated definition
    DAnnotation (With (Type Parameter ())) (Definition a k t)
  | -- | Type definition
    DType Name [Parameter ()] [Constructor Parameter () (Type Parameter ())]
  | -- | Codata type definition
    DCodata Name [Parameter ()] [(Name, Type Parameter ())]
  | -- | Function definition
    DFunction Name (Function Expression a t)
  | -- | Other (constant) top-level definitions
    DConstant Name (Constant Expression a t)
  | -- | Stand-alone type signature
    DSignature Name (With (Type Parameter ()))
  | -- | Import statement
    DImport Path [Name]
  | -- | Trait
    DTrait Name [Trait t] (Type Parameter ()) [(Name, Type Parameter ())]
  | -- | Trait instance
--    DInstance Name (Type Parameter k) [Definition a k t]
    DInstance2 Name (Type Parameter ()) [Definition a k t]
  | -- | Trait instance
    DTypeAlias Name [Parameter ()] (Type Parameter ())
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

definitionName :: Definition a k t -> Name
definitionName =
  \case
    DFunction name _ ->
      name
    DConstant name _ ->
      name
    DAnnotation _ d ->
      definitionName d
    DCodata name _ _ ->
      name
    _ ->
      error "Not implemented"
