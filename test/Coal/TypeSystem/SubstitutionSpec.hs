{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.SubstitutionSpec where

import Coal.Common.List1
import Coal.Language
import Coal.TypeSystem.Substitution
import Coal.TypeSystem.Unification
import Control.Monad (forM_)
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Test.Hspec

data SubstitutionSpecTestCase = SubstitutionSpecTestCase
  { substitution :: Substitution
  , input :: IndexedType
  , expected :: IndexedType
  }

substitutionSpec :: SpecWith ()
substitutionSpec =
  describe "Substitution tests" $ do
    undefined
