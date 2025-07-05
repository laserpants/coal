{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Noll.Compiler2Spec where

import Control.Monad ((>=>))
import Control.Monad.Identity (runIdentity)
import Data.Text (Text)
import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Lang.Lowpass.Language (Module (..), Object (..), opaque)
import Lang.Utils (Name)
import Noll.Compiler2
import Noll.Compiler2.Internal
import Text.RawString.QQ

-- import Noll.Compiler2Examples.Test02 (bazz)
import Noll.Language (Constructor (..), IndexedType (..), Intrinsic (..), Kind (..), Parameter (..), Row (..), Scheme (..), Type (..), TypeIndex (..))
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..))
import Noll.SystemF
import Noll.SystemFSpec.TestRunner
import Test.Hspec (Spec, describe, it)

import qualified Data.Set as Set
import qualified Lang.Common.Environment as Environment
import qualified Lang.Lowpass.Compiler as Lowpass
import qualified Lang.Lowpass.Compiler.Utils as Lowpass
import qualified Lang.Lowpass.Language as Lowpass
import qualified Noll.Set10.Test01
import qualified Noll.Set11.Test01
import qualified Noll.Set7.Test01
import qualified Noll.Set8.Test01
import qualified Noll.Set9.Test01

spec :: Spec
spec =
  describe "Noll.Compiler2" $ do
    it "" $ do
      1 == 2

compiler2TestEnvironment =
  Compiler2Environment
    { compiler2DataConstructorEnv = env1
    , compiler2TypeConstructorEnv = env2
    , compiler2TraitEnvironment = env3
    , compiler2TraitEnv = env4
    , compiler2AliasEnv = env5
    }

env1 =
  Environment.fromList
    [
      ( "LessThan"
      , Constructor
          "LessThan"
          0
          (Forall mempty [] (TConstructor KType "Ordering"))
      )
    ,
      ( "GreaterThan"
      , Constructor
          "GreaterThan"
          0
          (Forall mempty [] (TConstructor KType "Ordering"))
      )
    ,
      ( "EqualTo"
      , Constructor
          "EqualTo"
          0
          (Forall mempty [] (TConstructor KType "Ordering"))
      )
    ,
      ( "Node"
      , Constructor
          "Node"
          3
          ( Forall
              (Set.fromList [TypeIndex KType 0])
              []
              ( tvariable0
                  `TArrow` tree0
                  `TArrow` tree0
                  `TArrow` tree0
              )
          )
      )
    ,
      ( "Leaf"
      , Constructor
          "Leaf"
          0
          ( Forall
              (Set.fromList [TypeIndex KType 0])
              []
              tree0
          )
      )
    ,
      ( "Succ"
      , Constructor
          "Succ"
          1
          (Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat))
      )
    ,
      ( "Zero"
      , Constructor
          "Zero"
          0
          (Forall mempty [] (TIntrinsic INat))
      )
    ]

env2 =
  Environment.fromList
    [
      ( "Tree"
      , KArrow KType KType
      )
    ]

env3 =
  Environment.fromList
    [
      ( "Numeric"
      ,
        ( TypeIndex KType 0
        , Environment.fromList
            [
              ( "from_int32"
              , Forall
                  (Set.fromList [TypeIndex KType 0])
                  []
                  ( TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)
                  )
              )
            ]
        )
      )
    ,
      ( "Ordered"
      ,
        ( TypeIndex KType 0
        , Environment.fromList
            [
              ( "compare"
              , Forall
                  (Set.fromList [TypeIndex KType 0])
                  []
                  ( TVariable (TypeIndex KType 0)
                      `TArrow` TVariable (TypeIndex KType 0)
                      `TArrow` TConstructor KType "Ordering"
                  )
              )
            ]
        )
      )
    ]

env4 =
  Environment.fromList
    [
      ( "Numeric"
      ,
        ( TypeIndex KType 0
        , Environment.fromList
            [
              ( "from_int32"
              , Forall
                  (Set.fromList [TypeIndex KType 0])
                  []
                  ( TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)
                  )
              )
            ]
        )
      )
    ,
      ( "Ordered"
      ,
        ( TypeIndex KType 0
        , Environment.fromList
            [
              ( "compare"
              , Forall
                  (Set.fromList [TypeIndex KType 0])
                  []
                  ( TVariable (TypeIndex KType 0)
                      `TArrow` TVariable (TypeIndex KType 0)
                      `TArrow` TConstructor KType "Ordering"
                  )
              )
            ]
        )
      )
    ]

env5 =
  Environment.fromList
    [
      ( "Predicate"
      ,
        ( ["a"]
        , TVariable (Parameter () "a") `TArrow` TIntrinsic IBool
        )
      )
    ,
      ( "Range"
      ,
        ( ["a"]
        , TIntrinsic
            ( IRecord
                ( TRow
                    ( RExtend
                        "max"
                        (TVariable (Parameter () "a"))
                        ( RExtend
                            "min"
                            (TVariable (Parameter () "a"))
                            RNil
                        )
                    )
                )
            )
        )
      )
    ]

tree0 :: IndexedType
tree0 =
  TApplication
    KType
    (TConstructor (KArrow KType KType) "Tree")
    (TVariable (TypeIndex KType 0) :| [])

tvariable0 :: IndexedType
tvariable0 = TVariable (TypeIndex KType 0)

tvariable1 :: IndexedType
tvariable1 = TVariable (TypeIndex KType 1)

bool :: IndexedType
bool = TIntrinsic IBool

-- abc1 :: Module () Kind IndexedType
-- abc1 = fst (runIdentity (runCompiler2T compiler2TestEnvironment ((typePass >=> mainPass) Noll.Set7.Test01.moduleMain)))

abc2 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
abc2 = fst (runIdentity (runCompiler2T compiler2TestEnvironment (compileModule Noll.Set7.Test01.moduleMain)))

abc3 :: IO ()
abc3 = Lowpass.testModules =<< Lowpass.compileModules [moduleCore1, abc2]

--

abc4 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
abc4 = fst (runIdentity (runCompiler2T compiler2TestEnvironment (compileModule Noll.Set8.Test01.moduleMain)))

abc5 :: IO ()
abc5 = Lowpass.testModules =<< Lowpass.compileModules [moduleCore1, abc4]

--

abc6 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
abc6 = fst (runIdentity (runCompiler2T compiler2TestEnvironment (compileModule Noll.Set9.Test01.moduleUtilities)))

abc7 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
abc7 = fst (runIdentity (runCompiler2T compiler2TestEnvironment (compileModule Noll.Set9.Test01.moduleMain)))

abc8 :: IO ()
abc8 = Lowpass.testModules =<< Lowpass.compileModules [moduleCore1, abc6, abc7]

--

abc9 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
abc9 = fst (runIdentity (runCompiler2T compiler2TestEnvironment (compileModule Noll.Set10.Test01.moduleUtilities)))

abc10 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
abc10 = fst (runIdentity (runCompiler2T compiler2TestEnvironment (compileModule Noll.Set10.Test01.moduleMain)))

abc11 :: IO ()
abc11 = Lowpass.testModules =<< Lowpass.compileModules [moduleCore1, abc9, abc10]

--

abc12 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
abc12 = fst (runIdentity (runCompiler2T compiler2TestEnvironment (compileModule Noll.Set11.Test01.moduleMain)))

abc13 :: IO ()
abc13 = Lowpass.testModules =<< Lowpass.compileModules [moduleCore1, abc12]

moduleCore1 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
moduleCore1 = Lowpass.unsafeParseExpr <$> moduleCore

moduleCore :: Lowpass.Module Lowpass.Type Name Text
moduleCore =
  Lowpass.Module
    { moduleName = "Core$"
    , moduleImports =
        []
    , moduleObjects =
        [ OFunction
            "Core$.operator__not"
            [ Label Lowpass.bool "a"
            ]
            [r| 
                  if (a : bool) then false else true
              |]
        , OFunction
            "Core$.operator__reverse_composition"
            [ Label (Lowpass.opaque `Lowpass.arrow` Lowpass.opaque) "f"
            , Label (Lowpass.opaque `Lowpass.arrow` Lowpass.opaque) "g"
            , Label Lowpass.opaque "x"
            ]
            [r| 
                  @<*>(f : */*, @<*>(g : */*, x : *))
              |]
        , OFunction
            "Core$.operator__reverse_application"
            [ Label Lowpass.opaque "x"
            , Label (Lowpass.opaque `Lowpass.arrow` Lowpass.opaque) "f"
            ]
            [r| 
                  @<*>(f : */*, x : *)
              |]
        , OFunction
            "Core$.always"
            [ Label Lowpass.opaque "a"
            , Label Lowpass.opaque "_"
            ]
            [r|   
                  a : *
              |]
        , OFunction
            "Core$.operator__list_concatenation"
            [ Label (Lowpass.TCon "list" [Lowpass.opaque]) "xs"
            , Label (Lowpass.TCon "list" [Lowpass.opaque]) "ys"
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
            "Core$.trace_int32"
            [ Label Lowpass.int32 "n"
            ]
            [r|
                  #(print_int32 : int32/*, n : int32) (fn(a : *) => a : *)
              |]
        , OFunction
            "Core$.trace_string"
            [ Label Lowpass.string "s"
            ]
            [r|
                  #(print_string : string/*, s : string) (fn(a : *) => a : *)
              |]
        , OFunction
            "Core$.operator__string_concatenation"
            [ Label Lowpass.string "s"
            , Label Lowpass.string "t"
            ]
            [r|
                  #(string_concat : string/string/string, s : string, t : string) (fn(r : string) => r : string)
              |]
        , OFunction
            "Core$.int32_to_string"
            [ Label Lowpass.int32 "n"
            ]
            [r| 
                  #(int32_to_string : int32/string, n : int32) (fn(r : string) => r : string)
              |]
        , OFunction
            "Core$.pair_to_string"
            [ Label (Lowpass.TCon "Traceable" [Lowpass.TOpq]) "$dict1"
            , Label (Lowpass.TCon "Traceable" [Lowpass.TOpq]) "$dict2"
            , Label (Lowpass.TCon "$Tuple2" [Lowpass.TOpq, Lowpass.TOpq]) "p"
            ]
            [r| 
                  match<string>
                    ( p : $Tuple2(*,*) ) { 
                      | ( $Tuple2 : */*/$Tuple2(*,*)
                        , a : *
                        , b : *
                        ) =>
                          @<string>
                            ( Core$.operator__string_concatenation : string/string/string
                            , @<string>
                                ( Core$.operator__string_concatenation : string/string/string
                                , "("
                                , @<string>
                                    ( Core$.operator__string_concatenation : string/string/string
                                    , @<string>
                                        ( Core$.operator__string_concatenation : string/string/string
                                        , @<string>
                                            ( Core$.trace : Traceable(*)/*/string
                                            , $dict1 : Traceable(*)
                                            , a : *
                                            )
                                        , ","
                                        )
                                    , @<string>
                                        ( Core$.trace : Traceable(*)/*/string
                                        , $dict2 : Traceable(*)
                                        , b : *
                                        )
                                    )
                                )
                            , ")"
                            )
                    }
              |]
        , OFunction
            "Core$.list_to_string"
            [ Label (Lowpass.TCon "Traceable" [Lowpass.TOpq]) "$dict1"
            , Label (Lowpass.TCon "list" [Lowpass.TOpq]) "ls"
            ]
            [r| 
                  let
                    f : bool/list(*)/string =
                      fn(first : bool, l : list(*)) =>
                        match<string>
                          ( l : list(*)
                          ) {
                            | ( $Cons : */list(*)/list(*)
                              , x : *
                              , xs : list(*)
                              ) =>
                                @<string>
                                  ( Core$.operator__string_concatenation : string/string/string
                                  , if (first : bool) then "" else ","
                                  , @<string>
                                      ( Core$.operator__string_concatenation : string/string/string
                                      , @<string>
                                          ( Core$.trace : Traceable(*)/*/string
                                          , $dict1 : Traceable(*)
                                          , x : *
                                          )
                                      , @<string>
                                          ( f : list(*)/string
                                          , false
                                          , xs : list(*)
                                          )
                                      )
                                  )
                            | ( $Nil : list(*)
                              ) =>
                                ""
                          }
                    in
                      @<string>
                        ( Core$.operator__string_concatenation : string/string/string
                        , @<string>
                            ( Core$.operator__string_concatenation : string/string/string
                            , "["
                            , @<string>
                                ( f : list(*)/string
                                , true
                                , ls : list(*)
                                )
                            )
                        , "]"
                        )
              |]
        , OFunction
            "Core$.trace"
            [ Label (Lowpass.TCon "Traceable" [opaque]) "$a"
            ]
            [r| 
                  match<*>($a : Traceable(*)) {
                    | ( $Record : { trace : * | * }/Traceable(*)
                      , $r : { trace : * | * }
                      ) =>
                        select
                          { trace = $f : * | _ : * } =
                            $r : { trace : * | * }
                          in
                            $f : *
                  }
              |]
        , OFunction
            "Core$.unpack_nat"
            [ Label (Lowpass.TCon "$Nat" []) "nat"
            ]
            [r| 
                  match<int32>(nat: $Nat) {
                    | ( $Succ : int32/$Nat
                      , succ : int32
                      ) =>
                        [+ int32](succ : int32, 1)
                    | ( $Zero : $Nat
                      ) =>
                        0
                  }
              |]
        , OFunction
            "Core$.pack_nat"
            [ Label (Lowpass.int32) "n"
            ]
            [r| 
                  if ([== int32](n : int32, 0))
                    then
                      $Zero : $Nat
                    else
                      @<$Nat>
                        ( $Succ : int32/$Nat
                        , [- int32](n : int32, 1)
                        )
              |]
        ]
    }
