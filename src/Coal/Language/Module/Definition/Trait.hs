{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Trait (TraitDef (..)) where

import Coal.Language.Trait (Trait (..))
import Coal.Language.Type (Parameter, ParameterizedType)
import Coal.Language.Type.Kind (Kind (..))
import Coal.Language.Type.Scheme (Scheme (..))
import Data.Data (Data, Typeable)
import Extras (Name)

data TraitDef t = TraitDef [Trait t] (Parameter Kind) [(Name, Scheme Parameter () ParameterizedType)]
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
