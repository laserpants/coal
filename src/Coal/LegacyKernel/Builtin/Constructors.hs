{-# LANGUAGE OverloadedStrings #-}

module Coal.LegacyKernel.Builtin.Constructors (builtinConstructors) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment

builtinConstructors :: Environment Int
builtinConstructors =
  Environment.fromList
    [ ("$Cons", 0)
    , ("$Nil", 1)
    , ("$Succ", 0)
    , ("$Zero", 1)
    , ("Some", 1)
    , ("None", 0)
    , ("Machine", 0)
    ]
