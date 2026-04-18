{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

{- |
Module: Coal.Language.HasActive

Type class for extracting active type variables from language constructs.
-}
module Coal.Language.HasActive (HasActive (..), activeIdsIn) where

import Coal.Language.Type (TypeIndex (typeIndexId))
import Coal.Language.Type.Kind (Kind (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Extras (Map, Set)
import Extras.Data.Set (unionMap)

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
