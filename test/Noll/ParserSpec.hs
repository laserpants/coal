{-# LANGUAGE OverloadedStrings #-}

module Noll.ParserSpec where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Module
import Noll.Parser
import Noll.Parser.Expression
import Noll.Parser.Module
import Test.Hspec (Spec, describe, it)
import Text.Megaparsec (runParser)

spec :: Spec
spec =
  describe "Noll.Compiler" $ do
    it "" $ do
      runParser expressionParser "" "fold(pack_nat(n)) { | Zero => 1 | Succ(@f) as m => unpack_nat(m) * f }"
        == Right
          ( EFold
              ()
              ()
              ( EApplication
                  ()
                  ()
                  (EVariable () (Label () "pack_nat"))
                  ( EVariable () (Label () "n")
                      :| []
                  )
                  :| []
              )
              ( EClause
                  ()
                  ( PConstructor
                      ()
                      (Label () "Zero")
                      []
                  )
                  ( CPlain
                      ()
                      []
                      ( EApplication
                          ()
                          ()
                          (EVariable () (Label () "from_int32"))
                          (ELiteral () (LInt32 1) :| [])
                      )
                      :| []
                  )
                  <| EClause
                    ()
                    ( PAs
                        ()
                        (Label () "m")
                        ( PConstructor
                            ()
                            (Label () "Succ")
                            [ PAtVariable () (Label () "f")
                            ]
                        )
                    )
                    ( CPlain
                        ()
                        []
                        ( EApplication
                            ()
                            ()
                            (EBinaryOperator () () OMultiplication)
                            ( EApplication
                                ()
                                ()
                                (EVariable () (Label () "unpack_nat"))
                                ( EVariable () (Label () "m")
                                    :| []
                                )
                                <| EVariable () (Label () "f")
                                :| []
                            )
                        )
                        :| []
                    )
                  :| []
              )
              Nothing
          )
    it "" $ do
      runParser functionParser "" "fn factorial(n) = fold(pack_nat(n)) { | Zero => 1 | Succ(@f) as m => unpack_nat(m) * f };"
        == Right
          ( DFunction
              "factorial"
              ( Function
                  ()
                  (With [] ())
                  (PVariable () (Label () "n") :| [])
                  ( EFold
                      ()
                      ()
                      ( EApplication
                          ()
                          ()
                          (EVariable () (Label () "pack_nat"))
                          ( EVariable () (Label () "n")
                              :| []
                          )
                          :| []
                      )
                      ( EClause
                          ()
                          ( PConstructor
                              ()
                              (Label () "Zero")
                              []
                          )
                          ( CPlain
                              ()
                              []
                              ( EApplication
                                  ()
                                  ()
                                  (EVariable () (Label () "from_int32"))
                                  (ELiteral () (LInt32 1) :| [])
                              )
                              :| []
                          )
                          <| EClause
                            ()
                            ( PAs
                                ()
                                (Label () "m")
                                ( PConstructor
                                    ()
                                    (Label () "Succ")
                                    [ PAtVariable () (Label () "f")
                                    ]
                                )
                            )
                            ( CPlain
                                ()
                                []
                                ( EApplication
                                    ()
                                    ()
                                    (EBinaryOperator () () OMultiplication)
                                    ( EApplication
                                        ()
                                        ()
                                        (EVariable () (Label () "unpack_nat"))
                                        ( EVariable () (Label () "m")
                                            :| []
                                        )
                                        <| EVariable () (Label () "f")
                                        :| []
                                    )
                                )
                                :| []
                            )
                          :| []
                      )
                      Nothing
                  )
              ) ::
              Definition () () ()
          )
    it "" $ do
      runParser functionParser "" "fn factorial(n : int32) = fold(pack_nat(n)) { | Zero => 1 | Succ(@f) as m => unpack_nat(m) * f };"
        == Right
          ( DFunction
              "factorial"
              ( Function
                  ()
                  (With [] ())
                  ( PAnnotation
                      ()
                      (TIntrinsic IInt32)
                      (PVariable () (Label () "n"))
                      :| []
                  )
                  ( EFold
                      ()
                      ()
                      ( EApplication
                          ()
                          ()
                          (EVariable () (Label () "pack_nat"))
                          ( EVariable () (Label () "n")
                              :| []
                          )
                          :| []
                      )
                      ( EClause
                          ()
                          ( PConstructor
                              ()
                              (Label () "Zero")
                              []
                          )
                          ( CPlain
                              ()
                              []
                              ( EApplication
                                  ()
                                  ()
                                  (EVariable () (Label () "from_int32"))
                                  (ELiteral () (LInt32 1) :| [])
                              )
                              :| []
                          )
                          <| EClause
                            ()
                            ( PAs
                                ()
                                (Label () "m")
                                ( PConstructor
                                    ()
                                    (Label () "Succ")
                                    [ PAtVariable () (Label () "f")
                                    ]
                                )
                            )
                            ( CPlain
                                ()
                                []
                                ( EApplication
                                    ()
                                    ()
                                    (EBinaryOperator () () OMultiplication)
                                    ( EApplication
                                        ()
                                        ()
                                        (EVariable () (Label () "unpack_nat"))
                                        ( EVariable () (Label () "m")
                                            :| []
                                        )
                                        <| EVariable () (Label () "f")
                                        :| []
                                    )
                                )
                                :| []
                            )
                          :| []
                      )
                      Nothing
                  )
              ) ::
              Definition () () ()
          )
    it "" $ do
      runParser functionParser "" "fn factorial(n : int32) : int32 = fold(pack_nat(n)) { | Zero => 1 | Succ(@f) as m => unpack_nat(m) * f };"
        == Right
          ( DAnnotation
              (With [] (TIntrinsic IInt32))
              ( DFunction
                  "factorial"
                  ( Function
                      ()
                      (With [] ())
                      ( PAnnotation
                          ()
                          (TIntrinsic IInt32)
                          (PVariable () (Label () "n"))
                          :| []
                      )
                      ( EFold
                          ()
                          ()
                          ( EApplication
                              ()
                              ()
                              (EVariable () (Label () "pack_nat"))
                              ( EVariable () (Label () "n")
                                  :| []
                              )
                              :| []
                          )
                          ( EClause
                              ()
                              ( PConstructor
                                  ()
                                  (Label () "Zero")
                                  []
                              )
                              ( CPlain
                                  ()
                                  []
                                  ( EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "from_int32"))
                                      (ELiteral () (LInt32 1) :| [])
                                  )
                                  :| []
                              )
                              <| EClause
                                ()
                                ( PAs
                                    ()
                                    (Label () "m")
                                    ( PConstructor
                                        ()
                                        (Label () "Succ")
                                        [ PAtVariable () (Label () "f")
                                        ]
                                    )
                                )
                                ( CPlain
                                    ()
                                    []
                                    ( EApplication
                                        ()
                                        ()
                                        (EBinaryOperator () () OMultiplication)
                                        ( EApplication
                                            ()
                                            ()
                                            (EVariable () (Label () "unpack_nat"))
                                            ( EVariable () (Label () "m")
                                                :| []
                                            )
                                            <| EVariable () (Label () "f")
                                            :| []
                                        )
                                    )
                                    :| []
                                )
                              :| []
                          )
                          Nothing
                      )
                  )
              ) ::
              Definition () () ()
          )
    it "" $ do
      runParser functionParser "" "fn main() = trace_int32(factorial(12));"
        == Right
          ( DFunction
              "main"
              ( Function
                  ()
                  (With [] ())
                  (PLiteral () LUnit :| [])
                  ( EApplication
                      ()
                      ()
                      (EVariable () (Label () "trace_int32"))
                      ( EApplication
                          ()
                          ()
                          (EVariable () (Label () "factorial"))
                          ( EApplication
                              ()
                              ()
                              (EVariable () (Label () "from_int32"))
                              (ELiteral () (LInt32 12) :| [])
                              :| []
                          )
                          :| []
                      )
                  )
              ) ::
              Definition () () ()
          )
    it "" $ do
      runParser moduleParser "" "module Main { import Utilities(factorial); fn main() = trace_int32(factorial(12)); }"
        == Right
          ( Module
              (Path ["Main"])
              ["*"]
              [ DImport (Path ["Utilities"]) ["factorial"]
              , DFunction
                  "main"
                  ( Function
                      ()
                      (With [] ())
                      (PLiteral () LUnit :| [])
                      ( EApplication
                          ()
                          ()
                          (EVariable () (Label () "trace_int32"))
                          ( EApplication
                              ()
                              ()
                              (EVariable () (Label () "factorial"))
                              ( EApplication
                                  ()
                                  ()
                                  (EVariable () (Label () "from_int32"))
                                  (ELiteral () (LInt32 12) :| [])
                                  :| []
                              )
                              :| []
                          )
                      )
                  )
              ] ::
              Module () () ()
          )
    it "" $ do
      runParser moduleParser "" "module Utilities(factorial) { fn factorial(n : int32) : int32 = fold(pack_nat(n)) { | Zero => 1 | Succ(@f) as m => unpack_nat(m) * f }; }"
        == Right
          ( Module
              (Path ["Utilities"])
              ["factorial"]
              [ DAnnotation
                  (With [] (TIntrinsic IInt32))
                  ( DFunction
                      "factorial"
                      ( Function
                          ()
                          (With [] ())
                          ( PAnnotation
                              ()
                              (TIntrinsic IInt32)
                              (PVariable () (Label () "n"))
                              :| []
                          )
                          ( EFold
                              ()
                              ()
                              ( EApplication
                                  ()
                                  ()
                                  (EVariable () (Label () "pack_nat"))
                                  ( EVariable () (Label () "n")
                                      :| []
                                  )
                                  :| []
                              )
                              ( EClause
                                  ()
                                  ( PConstructor
                                      ()
                                      (Label () "Zero")
                                      []
                                  )
                                  ( CPlain
                                      ()
                                      []
                                      ( EApplication
                                          ()
                                          ()
                                          (EVariable () (Label () "from_int32"))
                                          (ELiteral () (LInt32 1) :| [])
                                      )
                                      :| []
                                  )
                                  <| EClause
                                    ()
                                    ( PAs
                                        ()
                                        (Label () "m")
                                        ( PConstructor
                                            ()
                                            (Label () "Succ")
                                            [ PAtVariable () (Label () "f")
                                            ]
                                        )
                                    )
                                    ( CPlain
                                        ()
                                        []
                                        ( EApplication
                                            ()
                                            ()
                                            (EBinaryOperator () () OMultiplication)
                                            ( EApplication
                                                ()
                                                ()
                                                (EVariable () (Label () "unpack_nat"))
                                                ( EVariable () (Label () "m")
                                                    :| []
                                                )
                                                <| EVariable () (Label () "f")
                                                :| []
                                            )
                                        )
                                        :| []
                                    )
                                  :| []
                              )
                              Nothing
                          )
                      )
                  ) ::
                  Definition () () ()
              ]
          )
    it "" $ do
      runParser functionParser "" "fn main() = let xs = [5, 3, 7, 2, 1, 6, 4] : list(int32) in trace_int32(match(sort(xs)) { | [] => 12345 | (y :: _) => y });"
        == Right
          ( DFunction
              "main"
              ( Function
                  ()
                  (With [] ())
                  (PLiteral () LUnit :| [])
                  ( ELet
                      ()
                      ( BPattern
                          ()
                          (PVariable () (Label () "xs"))
                          ( EAnnotation
                              ()
                              ( TApplication
                                  ()
                                  (TConstructor () "list")
                                  (TIntrinsic IInt32 :| [])
                              )
                              ( EListLiteral
                                  ()
                                  ()
                                  [ EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "from_int32"))
                                      (ELiteral () (LInt32 5) :| [])
                                  , EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "from_int32"))
                                      (ELiteral () (LInt32 3) :| [])
                                  , EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "from_int32"))
                                      (ELiteral () (LInt32 7) :| [])
                                  , EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "from_int32"))
                                      (ELiteral () (LInt32 2) :| [])
                                  , EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "from_int32"))
                                      (ELiteral () (LInt32 1) :| [])
                                  , EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "from_int32"))
                                      (ELiteral () (LInt32 6) :| [])
                                  , EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "from_int32"))
                                      (ELiteral () (LInt32 4) :| [])
                                  ]
                              )
                          )
                          :| []
                      )
                      ( EApplication
                          ()
                          ()
                          (EVariable () (Label () "trace_int32"))
                          ( EMatch
                              ()
                              ()
                              ( EApplication
                                  ()
                                  ()
                                  (EVariable () (Label () "sort"))
                                  (EVariable () (Label () "xs") :| [])
                              )
                              ( EClause
                                  ()
                                  ( PListLiteral () () []
                                  )
                                  ( CPlain
                                      ()
                                      []
                                      ( EApplication
                                          ()
                                          ()
                                          (EVariable () (Label () "from_int32"))
                                          (ELiteral () (LInt32 12345) :| [])
                                      )
                                      :| []
                                  )
                                  <| EClause
                                    ()
                                    ( PListCons
                                        ()
                                        ()
                                        (PVariable () (Label () "y"))
                                        (PAny () ())
                                    )
                                    ( CPlain
                                        ()
                                        []
                                        (EVariable () (Label () "y"))
                                        :| []
                                    )
                                  :| []
                              )
                              :| []
                          )
                      )
                  )
              ) ::
              Definition () () ()
          )
