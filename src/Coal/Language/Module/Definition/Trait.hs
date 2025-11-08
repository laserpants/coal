{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Trait (TraitDef (..)) where

import Coal.Language.Trait (Trait (..))
import Coal.Language.Type (Parameter, ParameterizedType)
import Coal.Language.Type.Kind (Kind (..))
import Data.Data (Data, Typeable)
import Extras (Name)

-- TODO: (Name, Scheme Parameter () ParameterizedType) ?
data TraitDef t = TraitDef [Trait t] (Parameter Kind) [(Name, ParameterizedType)]
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
