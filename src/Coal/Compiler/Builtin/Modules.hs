{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Coal.Compiler.Builtin.Modules (builtinModules, builtinModulesPaths) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Extras (Name, for)

{-# INLINE builtinModulesPaths #-}
builtinModulesPaths :: [Name]
builtinModulesPaths = for builtinModules fst

builtinModules :: [(Text, ByteString)]
builtinModules =
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
    ( "Process"
    , $(embedFile "lang/Process.coal")
    )
  ,
    ( "Stream"
    , $(embedFile "lang/Stream.coal")
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
