{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Trait (TraitDef (..)) where

import Coal.Language.Trait (Trait (..))
import Coal.Language.Type (Parameter, ParameterizedType)
import Coal.Language.Type.Kind (Kind (..))
import Coal.Language.Type.Scheme (Scheme (..))
import Data.Data (Data, Typeable)
import Extras (Name)

data TraitDef = TraitDef [Trait ParameterizedType] (Parameter Kind) [(Name, Scheme Parameter () ParameterizedType)]
  deriving (Show, Eq, Ord, Read, Data, Typeable)
