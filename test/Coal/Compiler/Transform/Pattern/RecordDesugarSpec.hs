{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Transform.Pattern.RecordDesugarSpec where

import Coal.Common.Label (Label (..))
import Coal.Common.List1 (List1, NonEmpty (..))
import Coal.Language

import qualified Data.Map.Strict as Map

samplx :: Expression () IndexedType
samplx =
  EMatch
    ()
    (TIntrinsic IString)
    ( ERecord
        ()
        (TIntrinsic (IRecord (TRow (RExtend "msg" (TIntrinsic IString) RNil))))
        ( Map.fromList
            [
              ( "msg"
              , ELiteral () (LString "wat")
              )
            ]
        )
        Nothing
    )
    ( EClause
        ()
        ( PRecord
            ()
            (TIntrinsic (IRecord (TRow (RExtend "msg" (TIntrinsic IString) RNil))))
            ( Map.fromList
                [
                  ( "msg"
                  , PVariable () (Label (TIntrinsic IString) "msg")
                  )
                ]
            )
            Nothing
        )
        ( CPlain
            ()
            []
            (EVariable () (Label (TIntrinsic IString) "msg"))
            :| []
        )
        :| []
    )

samplx2 :: Expression () IndexedType
samplx2 =
  EMatch
    ()
    (TIntrinsic IString)
    ( ERecord
        ()
        (TIntrinsic (IRecord (TRow (RExtend "msg" (TIntrinsic IString) RNil))))
        ( Map.fromList
            [
              ( "msg"
              , ELiteral () (LString "wat")
              )
            ]
        )
        Nothing
    )
    ( EClause
        ()
        (PConstructor () (Label (TIntrinsic (IRecord (TRow (RExtend "msg" (TIntrinsic IString) RNil)))) "$Record") 
          [ PVariable () (Label (TRow (RExtend "msg" (TIntrinsic IString) RNil)) "$row")
          ]
        )
        ( CPlain
            ()
            []
            (
              EFocus 
                "msg"
                (Label (TIntrinsic IString) "xx")
                (Label (TRow RNil) "_")
                (EVariable () (Label (TRow (RExtend "msg" (TIntrinsic IString) RNil)) "$row"))
                (EVariable () (Label (TIntrinsic IString) "xx"))
            )
            :| []
        )

        --( PRecord
        --    ()
        --    (TIntrinsic (IRecord (TRow (RExtend "msg" (TIntrinsic IString) RNil))))
        --    ( Map.fromList
        --        [
        --          ( "msg"
        --          , PVariable () (Label (TIntrinsic IString) "msg")
        --          )
        --        ]
        --    )
        --    Nothing
        --)
        --( CPlain
        --    ()
        --    []
        --    (EVariable () (Label (TIntrinsic IString) "msg"))
        --    :| []
        --)
        -- :| []

        :| []
    )

sampl1 =
  EMatch
    ()
    (TVariable (TypeIndex KType 0))
    ( ERecord
        ()
        (TIntrinsic (IRecord (TRow (RExtend "b" (TIntrinsic (IRecord (TRow (RExtend "msg" (TIntrinsic IString) RNil)))) RNil))))
        ( Map.fromList
            [
              ( "b"
              , ERecord
                  ()
                  (TIntrinsic (IRecord (TRow (RExtend "msg" (TIntrinsic IString) RNil))))
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
            (TIntrinsic (IRecord (TRow (RExtend "b" (TIntrinsic (IRecord (TRow (RExtend "msg" (TIntrinsic IString) RNil)))) RNil))))
            ( Map.fromList
                [
                  ( "b"
                  , PRecord
                      ()
                      (TIntrinsic (IRecord (TRow (RExtend "msg" (TIntrinsic IString) RNil))))
                      ( Map.fromList
                          [
                            ( "msg"
                            , PVariable () (Label (TIntrinsic IString) "msg")
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
                (TVariable (TypeIndex KType 0))
                (EVariable () (Label (TIntrinsic IString `TArrow` TVariable (TypeIndex KType 0)) "trace_string"))
                ( EVariable () (Label (TIntrinsic IString) "msg")
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
    (TVariable (TypeIndex KType 0))
    ( ERecord
        ()
        (TIntrinsic (IRecord (TRow (RExtend "b" (TIntrinsic (IRecord (TRow (RExtend "msg" (TIntrinsic IString) RNil)))) RNil))))
        ( Map.fromList
            [
              ( "b"
              , ERecord
                  ()
                  (TIntrinsic (IRecord (TRow (RExtend "msg" (TIntrinsic IString) RNil))))
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
            (TIntrinsic (IRecord (TRow (RExtend "b" (TIntrinsic (IRecord (TRow (RExtend "msg" (TIntrinsic IString) RNil)))) RNil))))
            ( Map.fromList
                [
                  ( "b"
                  , PRecord
                      ()
                      (TIntrinsic (IRecord (TRow (RExtend "msg" (TIntrinsic IString) RNil))))
                      ( Map.fromList
                          [
                            ( "msg"
                            , PVariable () (Label (TIntrinsic IString) "msg")
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
                (TVariable (TypeIndex KType 0))
                (EVariable () (Label (TIntrinsic IString `TArrow` TVariable (TypeIndex KType 0)) "trace_string"))
                ( EVariable () (Label (TIntrinsic IString) "msg")
                    :| []
                )
            )
            :| []
        )
        :| []
    )

-- sampl2 =
--  EMatch
--    ()
--    ()
--    ( ERecord
--        ()
--        ()
--        ( Map.fromList
--            [
--              ( "b"
--              , ERecord
--                  ()
--                  ()
--                  ( Map.fromList
--                      [
--                        ( "msg"
--                        , ELiteral () (LString "wat")
--                        )
--                      ]
--                  )
--                  Nothing
--              )
--            ]
--        )
--        Nothing
--    )
--    ( EClause
--        ()
--        ( PVariable () (Label () "x"))
--        ( CPlain
--            ()
--            []
--            ( EApplication
--                ()
--                ()
--                (EVariable () (Label () "trace_string"))
--                ( EVariable () (Label () "msg")
--                    :| []
--                )
--            )
--            :| []
--        )
--        :| []
--    )
