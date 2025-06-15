{-# LANGUAGE OverloadedStrings #-}

module Noll.Set4.Test02 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Module as Module

prog4_02 :: [Module () () ()]
prog4_02 =
  [ moduleMain
  ]

moduleMain :: Module () () ()
moduleMain =
  Module.fromDefinitionList
    (Path ["Main"])
    []
    [ DImport (Path ["Core$"]) ["trace_string"]
    , DCodata
        "Stream"
        [Parameter () "a"]
        [
          ( "Head"
          , TVariable (Parameter () "a")
          )
        ,
          ( "Tail"
          , TApplication () (TConstructor () "Stream") (TVariable (Parameter () "a") :| [])
          )
        ]
    , DConstant
        "nats"
        ( Constant
            ()
            (With [] ())
            ( EApplication
                ()
                ()
                ( EUnfold
                    ()
                    ()
                    (Label () "Stream")
                    "f"
                    ( PAnnotation
                        ()
                        (TIntrinsic IInt32)
                        (PVariable () (Label () "n"))
                        :| []
                    )
                    ( Map.fromList
                        [
                          ( "Head"
                          , EVariable () (Label () "n")
                          )
                        ,
                          ( "Tail"
                          , EApplication
                              ()
                              ()
                              (EVariable () (Label () "f"))
                              ( EApplication
                                  ()
                                  ()
                                  (EBinaryOperator () () OAddition)
                                  ( EVariable () (Label () "n")
                                      <| ELiteral () (LInt32 1)
                                      :| []
                                  )
                                  :| []
                              )
                          )
                        ]
                    )
                    Nothing
                )
                (ELiteral () (LInt32 0) :| [])
            )
        )
    , DFunction
        "nth"
        ( Function
            ()
            (With [] ())
            (PVariable () (Label () "n") :| [])
            ( EFold
                ()
                ()
                (EVariable () (Label () "n") :| [])
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
                        ( ELambda
                            ()
                            (PVariable () (Label () "stream") :| [])
                            ( ECodataSelect
                                ()
                                (Label () "Head")
                                (EVariable () (Label () "stream"))
                                Nothing
                            )
                        )
                        :| []
                    )
                    <| EClause
                      ()
                      ( PConstructor
                          ()
                          (Label () "Succ")
                          [ PAtVariable () (Label () "f")
                          ]
                      )
                      ( CPlain
                          ()
                          []
                          ( ELambda
                              ()
                              (PVariable () (Label () "stream") :| [])
                              ( EApplication
                                  ()
                                  ()
                                  (EVariable () (Label () "f"))
                                  ( ECodataSelect
                                      ()
                                      (Label () "Tail")
                                      (EVariable () (Label () "stream"))
                                      Nothing
                                      :| []
                                  )
                              )
                          )
                          :| []
                      )
                    :| []
                )
                Nothing
            )
        )
    , DFunction
        "main"
        ( Function
            ()
            (With [] ())
            (PLiteral () LUnit :| [])
            ( ELet
                ()
                ( BPattern
                    ()
                    (PVariable () (Label () "v"))
                    ( EApplication
                        ()
                        ()
                        (EVariable () (Label () "nth"))
                        ( EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 5) :| [])
                            <| EVariable () (Label () "nats")
                            :| []
                        )
                    )
                    :| []
                )
                ( EApplication
                    ()
                    ()
                    (EVariable () (Label () "trace_int32"))
                    ( EVariable () (Label () "v")
                        :| []
                    )
                )
            )
        )
    ]
