{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Transform.Pattern.RecordDesugarSpec where

import Coal.Common.Label (Label (..))
import Coal.Common.List1 (List1, NonEmpty (..))
import Coal.Language

import qualified Data.Map.Strict as Map

sampl1 =
  EMatch
    ()
    ()
    ( ERecord
        ()
        ()
        ( Map.fromList
            [
              ( "b"
              , ERecord
                  ()
                  ()
                  ( Map.fromList
                      [
                        ( "msg"
                        , ELiteral () (LString "wat") 
                        )
                      ]
                  )
                  Nothing
              )
            ]
        )
        Nothing
    )
    ( EClause
        ()
        ( PRecord
            ()
            ()
            ( Map.fromList
                [
                  ( "b"
                  , PRecord
                      ()
                      ()
                      ( Map.fromList
                          [
                            ( "msg"
                            , PVariable () (Label () "msg")
                            )
                          ]
                      )
                      Nothing
                  )
                ]
            )
            Nothing
        )
        ( CPlain
            ()
            []
            ( EApplication
                ()
                ()
                (EVariable () (Label () "trace_string"))
                ( EVariable () (Label () "msg")
                    :| []
                )
            )
            :| []
        )
        :| []
    )

sampl2 =
  EMatch
    ()
    ()
    ( ERecord
        ()
        ()
        ( Map.fromList
            [
              ( "b"
              , ERecord
                  ()
                  ()
                  ( Map.fromList
                      [
                        ( "msg"
                        , ELiteral () (LString "wat") 
                        )
                      ]
                  )
                  Nothing
              )
            ]
        )
        Nothing
    )
    ( EClause
        ()
        ( PVariable () (Label () "x"))
        ( CPlain
            ()
            []
            ( EApplication
                ()
                ()
                (EVariable () (Label () "trace_string"))
                ( EVariable () (Label () "msg")
                    :| []
                )
            )
            :| []
        )
        :| []
    )
