{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Transform.Pattern.AsDesugarSpec where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Noll.Language
import Lang.Label (Label (..))

testPattern1 :: Pattern () ()
testPattern1 =
  PAs
      ()
      (Label () "m")
      ( PConstructor
          ()
          (Label () "Succ")
          [ PAtVariable () (Label () "f")
          ]
      )

testPattern2 :: Pattern () ()
testPattern2 =
  PTuple () ()
    (
      PAs
          ()
          (Label () "m")
          ( PConstructor
              ()
              (Label () "Succ")
              [ PAtVariable () (Label () "f")
              ]
          )
      <|
      PAs
          ()
          (Label () "n") ( PAny () ()) 
        :| []
      )


