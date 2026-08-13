{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Kernel.Language.Interface
Description: Serializable interface of a translated kernel module

A compact, serializable interface of a translated kernel module's exported
objects. It is stored in the incremental-compilation cache so that, when a
module is loaded from cache (@BCached@), freshly compiled modules that import
it can still reconstruct the codegen information they need (function signatures
and constants). Only functions and constants are captured here; data
constructors are handled separately via the cached constructor tags and DData
information.
-}
module Coal.Kernel.Language.Interface (ObjectInterface (..), moduleInterface) where

import Coal.Common.Name (Name)
import Coal.Kernel.Language.Expr (Expr (ELit))
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Prim (Prim)
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Language.Type.HasType (typeOf)
import Data.Binary (Binary)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)

{- | Codegen-relevant description of a single exported object.

'IFunction' stores the parameter types and result type of a 'DFunction';
'IConstant' stores the literal payload of a 'DConstant' whose body is a
primitive literal (@Just prim@), or 'Nothing' for any other constant (which is
compiled to a thunk).
-}
data ObjectInterface
  = IFunction [Type] Type
  | IConstant (Maybe Prim)
  deriving (Show, Eq, Ord, Generic)

instance Binary ObjectInterface

{- | Extract the codegen-relevant interface of a translated kernel module,
keyed by (fully qualified) object name.
-}
moduleInterface :: Module Type -> Map Name ObjectInterface
moduleInterface Module{moduleObjects} = Map.fromList (mapMaybe go moduleObjects)
 where
  go (DFunction _ name lls expr) =
    Just (name, IFunction (typeOf <$> lls) (typeOf expr))
  go (DConstant name (ELit prim)) =
    Just (name, IConstant (Just prim))
  go (DConstant name _) =
    Just (name, IConstant Nothing)
  go _ =
    Nothing
