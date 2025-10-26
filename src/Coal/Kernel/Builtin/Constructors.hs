{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Builtin.Constructors (builtinConstructors) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment

builtinConstructors :: Environment Int
builtinConstructors = Environment.fromList [("$Cons", 0), ("$Nil", 1), ("$Succ", 0), ("$Zero", 1)]
