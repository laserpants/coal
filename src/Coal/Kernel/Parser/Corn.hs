{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Compile-time embedding of kernel language (corn) objects.

Provides the @corn@ quasi-quoter and the equivalent splice function
'cornObject'.  Both parse a single kernel object — a function, constant, or data
declaration — at compile time and splice the resulting AST into the program.
Malformed kernel source therefore surfaces as a compilation error instead of a
run-time 'error'.

The whole object is written in kernel syntax, so the function name, its
parameter list and its body all come from a single source of truth:

@
objectList =
  [ [corn| Builtin$.not(a : bool) = if (a : bool) then false else true |]
  ]
@
-}
module Coal.Kernel.Parser.Corn (
  corn,
  cornObject,
) where

import qualified Data.Text as Text
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp, Lift (lift), Q)
import Text.Megaparsec (errorBundlePretty, runParser)

import Coal.Kernel.Language.Object (Object)
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Parser.Module (object_)

{- | Parse one kernel object from source text and splice the resulting AST.

The input must consist of exactly one object; trailing input is a parse error.
Use this instead of the 'corn' quasi-quoter when the object's name has to be
computed in Haskell and prepended to the source text.
-}
cornObject :: String -> Q Exp
cornObject src =
  case parseObject src of
    Left err -> fail err
    Right obj -> lift obj

-- | Quasi-quoter form of 'cornObject': @[corn| Name(param : type) = body |]@.
corn :: QuasiQuoter
corn =
  QuasiQuoter
    { quoteExp = cornObject
    , quotePat = unsupported "pattern"
    , quoteType = unsupported "type"
    , quoteDec = unsupported "declaration"
    }
 where
  unsupported what _ =
    fail ("corn: only expression quotes are supported, but found a " <> what <> " quote")

-- | Parse a single kernel object, formatting parse errors for 'fail'.
parseObject :: String -> Either String (Object Type)
parseObject src =
  case runParser object_ "<corn>" (Text.pack src) of
    Left err -> Left ("corn: " <> errorBundlePretty err)
    Right obj -> Right obj
