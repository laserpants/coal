{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Noll.Set2.Test13 where

import Data.Text (Text)
import Lang.Label (Label (..))
import Lang.Lowpass.Language
import Lang.Lowpass.Parser.Expr (expr)
import Lang.Utils (Name, (<$$>))
import Text.Megaparsec (eof, runParser)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.RawString.QQ

import qualified Data.Text as Text
import qualified Lang.Lowpass.Compiler as Lowpass
import qualified Lang.Lowpass.Compiler.Utils as Lowpass
import qualified Lang.Lowpass.Language as Lowpass

unsafeParseExpr :: Text -> Lowpass.Expr Lowpass.Type
unsafeParseExpr t =
  case runParser expr "" (Text.stripStart t) of
    Left e ->
      error (errorBundlePretty e)
    Right r ->
      r

moduleCore1 :: Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
moduleCore1 = unsafeParseExpr <$> moduleCore

moduleCore :: Module Lowpass.Type Name Text
moduleCore =
  Module
    { moduleName = "Core$"
    , moduleImports =
        []
    , moduleObjects =
        [ OFunction
            "Core$.operator__not"
            [ Label bool "a"
            ]
            [r| 
                  if (a : bool) then false else true
              |]
        , OFunction
            "Core$.operator__reverse_composition"
            [ Label (opaque `arrow` opaque) "f"
            , Label (opaque `arrow` opaque) "g"
            , Label opaque "x"
            ]
            [r| 
                  @<*>(f : */*, @<*>(g : */*, x : *))
              |]
        , OFunction
            "Core$.operator__reverse_application"
            [ Label opaque "x"
            , Label (opaque `arrow` opaque) "f"
            ]
            [r| 
                  @<*>(f : */*, x : *)
              |]
        , OFunction
            "Core$.always"
            [ Label opaque "a"
            , Label opaque "_"
            ]
            [r|   
                  a : *
              |]
        , OFunction
            "Core$.operator__list_concatenation"
            [ Label (TCon "list" [opaque]) "xs"
            , Label (TCon "list" [opaque]) "ys"
            ]
            [r| 
                  match<list(*)>(xs : list(*)) {
                    | ( $Cons : */list(*)/list(*)
                      , z : *
                      , zs : list(*)
                      ) =>
                        @<list(*)>
                          ( $Cons : */list(*)/list(*)
                          , z : *
                          , @<list(*)>
                              ( Core$.operator__list_concatenation : list(*)/list(*)/list(*)
                              , zs : list(*)
                              , ys : list(*)
                              )
                          )
                    | ( $Nil : list(*)
                      ) =>
                        ys : list(*)
                  }
              |]
        , OFunction
            "Core$.$trace_int32"
            [ Label int32 "n"
            ]
            [r|
                  #(print_int32 : int32/*, n : int32) (fn(a : *) => a : *)
              |]
        ]
    }

moduleFoo1 :: Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
moduleFoo1 = unsafeParseExpr <$> moduleFoo

moduleFoo :: Module Lowpass.Type Name Text
moduleFoo =
  Module
    { moduleName = "Foo"
    , moduleImports =
        []
    , moduleObjects =
        [
        ]
    }

fixture3 :: [Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)]
fixture3 =
  [ moduleCore1
  , moduleFoo1
  ]

mooz :: IO ()
mooz = Lowpass.testModules =<< Lowpass.compileModules fixture3
