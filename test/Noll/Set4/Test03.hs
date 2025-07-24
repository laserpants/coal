{-# LANGUAGE OverloadedStrings #-}

module Noll.Set4.Test03 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Language.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Language.Module as Module

prog4_03 :: [Module () k ()]
prog4_03 =
  [ moduleMain
  ]

-- Expand folds
moduleMain :: Module () k ()
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
    , --    , DType
      --        "$$Stream"
      --        [Parameter () "a"]
      --        [ Constructor
      --            "$$Stream"
      --            1
      --            ( Forall
      --                (Set.fromList [Parameter () "a"])
      --                []
      --                ( TRow ( RExtend "$$Head" (TArrow (TIntrinsic IUnit) (TVariable (Parameter () "a"))) ( RExtend "$$Tail" (TArrow (TIntrinsic IUnit) (TApplication () (TConstructor () "Stream") (TVariable (Parameter () "a") :| []))) RNil))
      --                    `TArrow` (TApplication () (TConstructor () "$$Stream") (TVariable (Parameter () "a") :| []))
      --                )
      --            )
      --        ]
      --      ,
      DConstant
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
                    ( Just
                        ( ERecursiveLet
                            ()
                            (PVariable () (Label () "$unfold.1"))
                            ( ELambda
                                ()
                                (PVariable () (Label () "n") :| [])
                                ( ECodataFields
                                    ()
                                    ()
                                    ( Map.fromList
                                        [
                                          ( "$$Head"
                                          , ELambda
                                              ()
                                              (PAny () () :| [])
                                              (EVariable () (Label () "n"))
                                          )
                                        ,
                                          ( "$$Tail"
                                          , ELambda
                                              ()
                                              (PAny () () :| [])
                                              ( EApplication
                                                  ()
                                                  ()
                                                  (EVariable () (Label () "$unfold.1"))
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
                                          )
                                        ]
                                    )
                                )
                            )
                            (EVariable () (Label () "$unfold.1"))
                        )
                    )
                )
                (ELiteral () (LInt32 0) :| [])
            )
        )
        -- ,
        --         DFunction
        --         "nth"
        --         ( Function
        --             ()
        --             (With [] ())
        --             (PVariable () (Label () "n") :| [])
        --             ( EFold
        --                 ()
        --                 ()
        --                 (EVariable () (Label () "n") :| [])
        --                 ( EClause
        --                     ()
        --                     ( PConstructor
        --                         ()
        --                         (Label () "Zero")
        --                         []
        --                     )
        --                     ( CPlain
        --                         ()
        --                         []
        --                         ( ELambda
        --                             ()
        --                             (PVariable () (Label () "stream") :| [])
        --                             ( ECodataSelect
        --                                 ()
        --                                 (Label () "Head")
        --                                 (EVariable () (Label () "stream"))
        --                                 ( Just
        --                                     ( EApplication
        --                                         ()
        --                                         ()
        --                                         (EVariable () (Label () "$$force_Head"))
        --                                         (EVariable () (Label () "stream") :| [])
        --                                     )
        --                                 )
        --                             )
        --                         )
        --                         :| []
        --                     )
        --                     <| EClause
        --                       ()
        --                       ( PConstructor
        --                           ()
        --                           (Label () "Succ")
        --                           [ PAtVariable () (Label () "f")
        --                           ]
        --                       )
        --                       ( CPlain
        --                           ()
        --                           []
        --                           ( ELambda
        --                               ()
        --                               (PVariable () (Label () "stream") :| [])
        --                               ( EApplication
        --                                   ()
        --                                   ()
        --                                   (EVariable () (Label () "f"))
        --                                   ( ECodataSelect
        --                                       ()
        --                                       (Label () "Tail")
        --                                       (EVariable () (Label () "stream"))
        --                                       ( Just
        --                                           ( EApplication
        --                                               ()
        --                                               ()
        --                                               (EVariable () (Label () "$$force_Tail"))
        --                                               (EVariable () (Label () "stream") :| [])
        --                                           )
        --                                       )
        --                                       :| []
        --                                   )
        --                               )
        --                           )
        --                           :| []
        --                       )
        --                     :| []
        --                 )
        --                 ( Just
        --                     ( ERecursiveLet
        --                         ()
        --                         (PVariable () (Label{labelTag = (), labelName = "$fold.1"}))
        --                         ( ELambda
        --                             ()
        --                             (PVariable () (Label{labelTag = (), labelName = "$fold.1.expr"}) :| [])
        --                             ( EMatch
        --                                 ()
        --                                 ()
        --                                 (EVariable () (Label{labelTag = (), labelName = "$fold.1.expr"}))
        --                                 ( EClause
        --                     ()
        --                     ( PConstructor
        --                         ()
        --                         (Label () "Zero")
        --                         []
        --                     )
        --                     ( CPlain
        --                         ()
        --                         []
        --                         ( ELambda
        --                             ()
        --                             (PVariable () (Label () "stream") :| [])
        --                             ( ECodataSelect
        --                                 ()
        --                                 (Label () "Head")
        --                                 (EVariable () (Label () "stream"))
        --                                 ( Just
        --                                     ( EApplication
        --                                         ()
        --                                         ()
        --                                         (EVariable () (Label () "$$force_Head"))
        --                                         (EVariable () (Label () "stream") :| [])
        --                                     )
        --                                 )
        --                             )
        --                         )
        --                         :| []
        --                     )
        --                     <| EClause
        --                       ()
        --                       ( PConstructor
        --                           ()
        --                           (Label () "Succ")
        --                           [ PVariable () (Label () "f")
        --                           ]
        --                       )
        --                       ( CPlain
        --                           ()
        --                           []
        --                           ( ELambda
        --                               ()
        --                               (PVariable () (Label () "stream") :| [])
        --                               ( EApplication
        --                                   ()
        --                                   ()
        --                                   (EVariable () (Label () "$fold.1"))
        --                                   ( EVariable () (Label () "f")
        --                                      <| ECodataSelect
        --                                       ()
        --                                       (Label () "Tail")
        --                                       (EVariable () (Label () "stream"))
        --                                       ( Just
        --                                           ( EApplication
        --                                               ()
        --                                               ()
        --                                               (EVariable () (Label () "$$force_Tail"))
        --                                               (EVariable () (Label () "stream") :| [])
        --                                           )
        --                                       )
        --                                       :| []
        --                                   )
        --                               )
        --                           )
        --                           :| []
        --                       )
        --                     :| []
        --                 )
        --                             )
        --                         )
        --                         ( EApplication
        --                             ()
        --                             ()
        --                             (EVariable () (Label{labelTag = (), labelName = "$fold.1"}))
        --                             (EVariable () (Label{labelTag = (), labelName = "n"}) :| [])
        --                         )
        --                     )
        --                 )
        --             )
        --         )
        -- , DFunction
        --   "main"
        --   ( Function
        --       ()
        --       (With [] ())
        --       (PLiteral () LUnit :| [])
        --       ( ELet
        --           ()
        --           ( BPattern
        --               ()
        --               (PVariable () (Label () "v"))
        --               ( EApplication
        --                   ()
        --                   ()
        --                   (EVariable () (Label () "nth"))
        --                   ( EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 5) :| [])
        --                       <| EVariable () (Label () "nats")
        --                       :| []
        --                   )
        --               )
        --               :| []
        --           )
        --           ( EApplication
        --               ()
        --               ()
        --               (EVariable () (Label () "trace_int32"))
        --               ( EVariable () (Label () "v")
        --                   :| []
        --               )
        --           )
        --       )
        --   )
    ]
