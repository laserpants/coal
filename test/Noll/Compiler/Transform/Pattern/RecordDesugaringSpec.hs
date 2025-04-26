{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Transform.Pattern.RecordDesugaringSpec where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Lang.Utils (Dictionary, Name)
import Noll.Compiler.Transform.Pattern.RecordDesugaring
import Noll.Language
import Test.Hspec (Spec, describe, it)

import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  undefined

foo123 :: (Expression () (Type TypeIndex Kind), [(Name, Dictionary (TypedPattern ()), Maybe (TypedPattern ()))])
foo123 = runExpandRecordPatterns (expandRecordPatterns fixtr1) "row" 1

foo456 = fst foo123 == fixtr2

fixtr1 =
  ( EMatch
      ()
      (TIntrinsic IBool)
      ( EVariable
          ()
          ( Label
              ( TIntrinsic
                  ( IRecord
                      ( TRow
                          ( RExtend
                              "max"
                              (TVariable (TypeIndex KType 0))
                              ( RExtend
                                  "min"
                                  (TVariable (TypeIndex KType 0))
                                  RNil
                              )
                          )
                      )
                  )
              )
              "$v.0"
          )
      )
      ( EClause
          ()
          ( PAnnotation
              ()
              ( TAlias
                  "Range"
                  [TVariable (Parameter () "a")]
                  ( TIntrinsic
                      ( IRecord
                          ( TRow
                              ( RExtend
                                  "max"
                                  (TVariable (Parameter () "a"))
                                  (RExtend "min" (TVariable (Parameter () "a")) RNil)
                              )
                          )
                      )
                  )
              )
              ( PRecord
                  ()
                  ( TIntrinsic
                      ( IRecord
                          ( TRow
                              ( RExtend
                                  "max"
                                  (TVariable (TypeIndex KType 0))
                                  ( RExtend
                                      "min"
                                      (TVariable (TypeIndex KType 0))
                                      RNil
                                  )
                              )
                          )
                      )
                  )
                  ( Map.fromList
                      [
                        ( "min"
                        , PVariable () (Label (TVariable (TypeIndex KType 0)) "min")
                        )
                      ,
                        ( "max"
                        , PVariable () (Label (TVariable (TypeIndex KType 0)) "max")
                        )
                      ]
                  )
                  Nothing
              )
          )
          ( CPlain
              ()
              []
              ( EApplication
                  ()
                  (TIntrinsic IBool)
                  (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalAnd)
                  ( EApplication
                      ()
                      (TIntrinsic IBool)
                      (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "greater_than"))
                      ( EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                          <| EVariable () (Label (TVariable (TypeIndex KType 0)) "min")
                          :| []
                      )
                      <| ( EApplication
                            ()
                            (TIntrinsic IBool)
                            (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalOr)
                         )
                        ( EApplication
                            ()
                            (TIntrinsic IBool)
                            (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
                            ( EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                                <| EVariable () (Label (TVariable (TypeIndex KType 0)) "max")
                                :| []
                            )
                            <| EApplication
                              ()
                              (TIntrinsic IBool)
                              ( EBinaryOperator
                                  ()
                                  (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                                  OEqualTo
                              )
                              ( EVariable () (Label (TVariable (TypeIndex KType 0)) "max")
                                  <| EApplication
                                    ()
                                    (TVariable (TypeIndex KType 0))
                                    (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)) "from_int32"))
                                    (ELiteral () (LInt32 (-1)) :| [])
                                  :| []
                              )
                            :| []
                        )
                      :| []
                  )
              )
              :| []
          )
          :| []
      )
  )

fixtr2 =
  ( EMatch
      ()
      (TIntrinsic IBool)
      ( EVariable
          ()
          ( Label
              ( TIntrinsic
                  ( IRecord
                      ( TRow
                          ( RExtend
                              "max"
                              (TVariable (TypeIndex KType 0))
                              ( RExtend
                                  "min"
                                  (TVariable (TypeIndex KType 0))
                                  RNil
                              )
                          )
                      )
                  )
              )
              "$v.0"
          )
      )
      ( EClause
          ()
          ( PAnnotation
              ()
              ( TAlias
                  "Range"
                  [TVariable (Parameter () "a")]
                  ( TIntrinsic
                      ( IRecord
                          ( TRow
                              ( RExtend
                                  "max"
                                  (TVariable (Parameter () "a"))
                                  (RExtend "min" (TVariable (Parameter () "a")) RNil)
                              )
                          )
                      )
                  )
              )
              ( PConstructor
                  ()
                  ( Label
                      ( TIntrinsic
                          ( IRecord
                              ( TRow
                                  ( RExtend
                                      "max"
                                      (TVariable (TypeIndex KType 0))
                                      ( RExtend
                                          "min"
                                          (TVariable (TypeIndex KType 0))
                                          RNil
                                      )
                                  )
                              )
                          )
                      )
                      "$Record"
                  )
                  [ PVariable
                      ()
                      ( Label
                          ( TRow
                              ( RExtend
                                  "max"
                                  (TVariable (TypeIndex KType 0))
                                  ( RExtend
                                      "min"
                                      (TVariable (TypeIndex KType 0))
                                      RNil
                                  )
                              )
                          )
                          "$row.1"
                      )
                  ]
              )
          )
          ( CPlain
              ()
              []
              ( EFocus
                  "max"
                  (Label (TVariable (TypeIndex KType 0)) "$row.1.field.max")
                  (Label (TIntrinsic (IRecord (TRow (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)))) "$row.1.tail")
                  ( EVariable
                      ()
                      ( Label
                          ( TRow
                              ( RExtend
                                  "max"
                                  (TVariable (TypeIndex KType 0))
                                  ( RExtend
                                      "min"
                                      (TVariable (TypeIndex KType 0))
                                      RNil
                                  )
                              )
                          )
                          "$row.1"
                      )
                  )
                  ( EMatch
                      ()
                      (TIntrinsic IBool)
                      (EVariable () (Label (TVariable (TypeIndex KType 0)) "$row.1.field.max"))
                      ( EClause
                          ()
                          (PVariable () (Label (TVariable (TypeIndex KType 0)) "max"))
                          ( CPlain
                              ()
                              []
                              ( EMatch
                                  ()
                                  (TIntrinsic IBool)
                                  (EVariable () (Label (TIntrinsic (IRecord (TRow (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)))) "$row.1.tail"))
                                  ( EClause
                                      ()
                                      ( PConstructor
                                          ()
                                          ( Label
                                              ( TIntrinsic
                                                  ( IRecord
                                                      ( TRow
                                                          ( RExtend
                                                              "min"
                                                              (TVariable (TypeIndex KType 0))
                                                              RNil
                                                          )
                                                      )
                                                  )
                                              )
                                              "$Record"
                                          )
                                          [ PVariable
                                              ()
                                              ( Label
                                                  ( TRow
                                                      ( RExtend
                                                          "min"
                                                          (TVariable (TypeIndex KType 0))
                                                          RNil
                                                      )
                                                  )
                                                  "$row.2"
                                              )
                                          ]
                                      )
                                      ( CPlain
                                          ()
                                          []
                                          ( EFocus
                                              "min"
                                              (Label (TVariable (TypeIndex KType 0)) "$row.2.field.min")
                                              (Label (TIntrinsic (IRecord (TRow RNil))) "$row.2.tail")
                                              ( EVariable
                                                  ()
                                                  ( Label
                                                      ( TRow
                                                          ( RExtend
                                                              "min"
                                                              (TVariable (TypeIndex KType 0))
                                                              RNil
                                                          )
                                                      )
                                                      "$row.2"
                                                  )
                                              )
                                              ( EMatch
                                                  ()
                                                  (TIntrinsic IBool)
                                                  (EVariable () (Label (TVariable (TypeIndex KType 0)) "$row.2.field.min"))
                                                  ( EClause
                                                      ()
                                                      (PVariable () (Label (TVariable (TypeIndex KType 0)) "min"))
                                                      ( CPlain
                                                          ()
                                                          []
                                                          ( EMatch
                                                              ()
                                                              (TIntrinsic IBool)
                                                              (EVariable () (Label (TIntrinsic (IRecord (TRow RNil))) "$row.2.tail"))
                                                              ( EClause
                                                                  ()
                                                                  ( PConstructor
                                                                      ()
                                                                      (Label (TIntrinsic (IRecord (TRow RNil))) "$Record")
                                                                      [ PVariable
                                                                          ()
                                                                          (Label (TRow RNil) "_")
                                                                      ]
                                                                  )
                                                                  ( CPlain
                                                                      ()
                                                                      []
                                                                      ( EApplication
                                                                          ()
                                                                          (TIntrinsic IBool)
                                                                          (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalAnd)
                                                                          ( EApplication
                                                                              ()
                                                                              (TIntrinsic IBool)
                                                                              (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "greater_than"))
                                                                              ( EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                                                                                  <| EVariable () (Label (TVariable (TypeIndex KType 0)) "min")
                                                                                  :| []
                                                                              )
                                                                              <| ( EApplication
                                                                                    ()
                                                                                    (TIntrinsic IBool)
                                                                                    (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalOr)
                                                                                 )
                                                                                ( EApplication
                                                                                    ()
                                                                                    (TIntrinsic IBool)
                                                                                    (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
                                                                                    ( EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                                                                                        <| EVariable () (Label (TVariable (TypeIndex KType 0)) "max")
                                                                                        :| []
                                                                                    )
                                                                                    <| EApplication
                                                                                      ()
                                                                                      (TIntrinsic IBool)
                                                                                      ( EBinaryOperator
                                                                                          ()
                                                                                          (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                                                                                          OEqualTo
                                                                                      )
                                                                                      ( EVariable () (Label (TVariable (TypeIndex KType 0)) "max")
                                                                                          <| EApplication
                                                                                            ()
                                                                                            (TVariable (TypeIndex KType 0))
                                                                                            (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)) "from_int32"))
                                                                                            (ELiteral () (LInt32 (-1)) :| [])
                                                                                          :| []
                                                                                      )
                                                                                    :| []
                                                                                )
                                                                              :| []
                                                                          )
                                                                      )
                                                                      :| []
                                                                  )
                                                                  :| []
                                                              )
                                                          )
                                                          :| []
                                                      )
                                                      :| []
                                                  )
                                              )
                                          )
                                          :| []
                                      )
                                      :| []
                                  )
                              )
                              :| []
                          )
                          :| []
                      )
                  )
              )
              :| []
          )
          :| []
      )
  )
