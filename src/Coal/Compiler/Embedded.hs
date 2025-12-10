{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Coal.Compiler.Embedded (embedded) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Text (Text)

embedded :: [(Text, ByteString)]
embedded =
  [
    ( "IO"
    , $(embedFile "lang/IO.coal")
    )
  ,
    ( "List"
    , $(embedFile "lang/List.coal")
    )
  ,
    ( "Nat"
    , $(embedFile "lang/Nat.coal")
    )
  ,
    ( "Number"
    , $(embedFile "lang/Number.coal")
    )
  ,
    ( "String"
    , $(embedFile "lang/String.coal")
    )
  ,
    ( "Char"
    , $(embedFile "lang/Char.coal")
    )
  ,
    ( "Option"
    , $(embedFile "lang/Option.coal")
    )
  ,
    ( "Coal.Combinators"
    , $(embedFile "lang/Coal/Combinators.coal")
    )
  ,
    ( "Coal.Monoid"
    , $(embedFile "lang/Coal/Monoid.coal")
    )
  ,
    ( "Coal.Functor"
    , $(embedFile "lang/Coal/Functor.coal")
    )
  ,
    ( "Coal.Applicative"
    , $(embedFile "lang/Coal/Applicative.coal")
    )
  ,
    ( "Coal.Monad"
    , $(embedFile "lang/Coal/Monad.coal")
    )
  ]
