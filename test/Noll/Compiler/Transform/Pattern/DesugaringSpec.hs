{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Transform.Pattern.DesugaringSpec where

import Noll.Common.List1 (NonEmpty ((:|)), (<|))
import Noll.Compiler.Transform.Pattern.Desugaring (expandPatterns, runPatternDesugaring)
import Noll.Label (Label (..))
import Noll.Language (
  BinaryOperator (..),
  Binding (..),
  Choice (..),
  Clause (..),
  Expression (..),
  Intrinsic (..),
  Kind (..),
  Parameter (..),
  Pattern (..),
  Row (..),
  Type (..),
  TypeIndex (..),
  Uses (..),
 )
import Noll.Language.Module.Function (Function (..))
import Test.Hspec (Spec, describe, it)

import qualified Data.Map.Strict as Map

spec :: Spec
spec =
  describe "" $ do
    it "" $
      runPatternDesugaring "v" 0 (expandPatterns fixture1) == fixture2
    it "" $
      runPatternDesugaring "v" 0 (expandPatterns fixture3) == fixture4
    it "" $
      runPatternDesugaring "v" 0 (expandPatterns fixture5) == fixture6
    it "" $
      runPatternDesugaring "v" 0 (expandPatterns fixture7) == fixture8
    it "" $
      runPatternDesugaring "v" 0 (expandPatterns fixture9) == fixture10
    it "" $
      runPatternDesugaring "v" 0 (expandPatterns fixture11) == fixture12

-- let
--  Some(p) = x     Option(int32)
--  in
--    p             int32
--
fixture1 :: Expression () (Type TypeIndex ())
fixture1 =
  ( ELet
      ()
      ( BPattern
          ()
          (PConstructor () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "Some") [PVariable () (Label (TIntrinsic IInt32) "p")])
          (EVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "x"))
          :| []
      )
      (EVariable () (Label (TIntrinsic IInt32) "p"))
  )

-- let
--  $v.0 = x             Option(int32)
--  in
--    match(             int32
--      $v.0             Option(int32)
--    ) {
--      | Some(p) =>     Option(int32)
--          p            int32
--    }
--
fixture2 :: Expression () (Type TypeIndex ())
fixture2 =
  ( ELet
      ()
      ( BPattern
          ()
          (PVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "$v.0"))
          (EVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "x"))
          :| []
      )
      ( EMatch
          ()
          (TIntrinsic IInt32)
          (EVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "$v.0"))
          ( EClause
              ()
              (PConstructor () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "Some") [PVariable () (Label (TIntrinsic IInt32) "p")])
              ( CPlain
                  ()
                  []
                  (EVariable () (Label (TIntrinsic IInt32) "p"))
                  :| []
              )
              :| []
          )
      )
  )

-- fn(Some(p)) =>        Option(int32)
--   p                   int32
--
fixture3 :: Expression () (Type TypeIndex ())
fixture3 =
  ELambda
    ()
    (PConstructor () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "Some") [PVariable () (Label (TIntrinsic IInt32) "p")] :| [])
    (EVariable () (Label (TIntrinsic IInt32) "p"))

-- fn($v.0) =>           Option(int32)
--   match(              int32
--     $v.0              Option(int32)
--   ) {
--     | Some(p) =>      Option(int32)
--         p             int32
--   }
--
fixture4 :: Expression () (Type TypeIndex ())
fixture4 =
  ELambda
    ()
    (PVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "$v.0") :| [])
    ( EMatch
        ()
        (TIntrinsic IInt32)
        (EVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "$v.0"))
        ( EClause
            ()
            (PConstructor () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "Some") [PVariable () (Label (TIntrinsic IInt32) "p")])
            ( CPlain
                ()
                []
                (EVariable () (Label (TIntrinsic IInt32) "p"))
                :| []
            )
            :| []
        )
    )

-- letrec
--  Some(p) = x     Option(int32)
--  in
--    p             int32
--
fixture5 :: Expression () (Type TypeIndex ())
fixture5 =
  ( ERecursiveLet
      ()
      (PConstructor () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "Some") [PVariable () (Label (TIntrinsic IInt32) "p")])
      (EVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "x"))
      (EVariable () (Label (TIntrinsic IInt32) "p"))
  )

-- let
--  $v.0 = x             Option(int32)
--  in
--    match(             int32
--      $v.0             Option(int32)
--    ) {
--      | Some(p) =>     Option(int32)
--          p            int32
--    }
--
fixture6 :: Expression () (Type TypeIndex ())
fixture6 =
  ( ELet
      ()
      ( BPattern
          ()
          (PVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "$v.0"))
          (EVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "x"))
          :| []
      )
      ( EMatch
          ()
          (TIntrinsic IInt32)
          (EVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "$v.0"))
          ( EClause
              ()
              (PConstructor () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "Some") [PVariable () (Label (TIntrinsic IInt32) "p")])
              ( CPlain
                  ()
                  []
                  (EVariable () (Label (TIntrinsic IInt32) "p"))
                  :| []
              )
              :| []
          )
      )
  )

--
-- let
--   in_range =
--     fn({ max, min } : Range(a), n : a) =>
--       gt(n, min) &&
--         (gt(min, max) || lte(n, max))
--   in
--     in_range
--
fixture7 :: Expression () (Type TypeIndex Kind)
fixture7 =
  ELet
    ()
    ( BPattern
        ()
        ( PVariable
            ()
            ( Label
                ( TIntrinsic
                    ( IRecord
                        (TRow (RExtend "max" (TVariable (TypeIndex KType 0)) (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)))
                    )
                    `TArrow` TVariable (TypeIndex KType 0)
                    `TArrow` TIntrinsic IBool
                )
                "in_range"
            )
        )
        ( ELambda
            ()
            ( PAnnotation
                ()
                ( TAlias
                    "Range"
                    [TVariable (Parameter () "a")]
                    ( TIntrinsic
                        ( IRecord
                            (TRow (RExtend "max" (TVariable (Parameter () "a")) (RExtend "min" (TVariable (Parameter () "a")) RNil)))
                        )
                    )
                )
                ( PRecord
                    ()
                    ( TIntrinsic
                        ( IRecord
                            (TRow (RExtend "max" (TVariable (TypeIndex KType 0)) (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)))
                        )
                    )
                    ( Map.fromList
                        [
                          ( "max"
                          , PShorthand () (Label (TVariable (TypeIndex KType 0)) "max")
                          )
                        ,
                          ( "min"
                          , PShorthand () (Label (TVariable (TypeIndex KType 0)) "min")
                          )
                        ]
                    )
                    Nothing
                )
                <| PAnnotation () (TVariable (Parameter () "a")) (PVariable () (Label (TVariable (TypeIndex KType 0)) "n"))
                :| []
            )
            ( EApplication
                ()
                (TIntrinsic IBool)
                ( EBinaryOperator
                    ()
                    ( TIntrinsic IBool
                        `TArrow` TIntrinsic IBool
                        `TArrow` TIntrinsic IBool
                    , OLogicalAnd
                    )
                )
                ( EApplication
                    ()
                    (TIntrinsic IBool)
                    ( EVariable
                        ()
                        ( Label
                            ( TVariable (TypeIndex KType 0)
                                `TArrow` TVariable (TypeIndex KType 0)
                                `TArrow` TIntrinsic IBool
                            )
                            "gt"
                        )
                    )
                    (EVariable () (Label (TVariable (TypeIndex KType 0)) "n") <| EVariable () (Label (TVariable (TypeIndex KType 0)) "min") :| [])
                    <| EApplication
                      ()
                      (TIntrinsic IBool)
                      ( EBinaryOperator
                          ()
                          ( TIntrinsic IBool
                              `TArrow` TIntrinsic IBool
                              `TArrow` TIntrinsic IBool
                          , OLogicalOr
                          )
                      )
                      ( EApplication
                          ()
                          (TIntrinsic IBool)
                          ( EVariable
                              ()
                              ( Label
                                  ( TVariable (TypeIndex KType 0)
                                      `TArrow` TVariable (TypeIndex KType 0)
                                      `TArrow` TIntrinsic IBool
                                  )
                                  "gt"
                              )
                          )
                          (EVariable () (Label (TVariable (TypeIndex KType 0)) "min") <| EVariable () (Label (TVariable (TypeIndex KType 0)) "max") :| [])
                          <| EApplication
                            ()
                            (TIntrinsic IBool)
                            ( EVariable
                                ()
                                ( Label
                                    ( TVariable (TypeIndex KType 0)
                                        `TArrow` TVariable (TypeIndex KType 0)
                                        `TArrow` TIntrinsic IBool
                                    )
                                    "lte"
                                )
                            )
                            (EVariable () (Label (TVariable (TypeIndex KType 0)) "n") <| EVariable () (Label (TVariable (TypeIndex KType 0)) "max") :| [])
                          :| []
                      )
                    :| []
                )
            )
        )
        :| []
    )
    ( EVariable
        ()
        ( Label
            ( TIntrinsic
                ( IRecord
                    (TRow (RExtend "max" (TVariable (TypeIndex KType 1)) (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)))
                )
                `TArrow` TVariable (TypeIndex KType 1)
                `TArrow` TIntrinsic IBool
            )
            "in_range"
        )
    )

--
-- let
--   in_range =
--     fn($v.0, n : a) =>
--       match($v.0) {
--         | { max, min } : Range(a) =>
--             gt(n, min) && (gt(min, max) || lte(n, max))
--       }
--   in
--     in_range
--
fixture8 :: Expression () (Type TypeIndex Kind)
fixture8 =
  ELet
    ()
    ( BPattern
        ()
        ( PVariable
            ()
            ( Label
                ( TIntrinsic
                    ( IRecord
                        (TRow (RExtend "max" (TVariable (TypeIndex KType 0)) (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)))
                    )
                    `TArrow` TVariable (TypeIndex KType 0)
                    `TArrow` TIntrinsic IBool
                )
                "in_range"
            )
        )
        ( ELambda
            ()
            ( PVariable
                ()
                ( Label
                    ( TIntrinsic
                        ( IRecord
                            (TRow (RExtend "max" (TVariable (TypeIndex KType 0)) (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)))
                        )
                    )
                    "$v.0"
                )
                <| PAnnotation () (TVariable (Parameter () "a")) (PVariable () (Label (TVariable (TypeIndex KType 0)) "n"))
                :| []
            )
            ( EMatch
                ()
                (TIntrinsic IBool)
                ( EVariable
                    ()
                    ( Label
                        ( TIntrinsic
                            ( IRecord
                                (TRow (RExtend "max" (TVariable (TypeIndex KType 0)) (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)))
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
                                    (TRow (RExtend "max" (TVariable (Parameter () "a")) (RExtend "min" (TVariable (Parameter () "a")) RNil)))
                                )
                            )
                        )
                        ( PRecord
                            ()
                            ( TIntrinsic
                                ( IRecord
                                    (TRow (RExtend "max" (TVariable (TypeIndex KType 0)) (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)))
                                )
                            )
                            ( Map.fromList
                                [
                                  ( "max"
                                  , PShorthand () (Label (TVariable (TypeIndex KType 0)) "max")
                                  )
                                ,
                                  ( "min"
                                  , PShorthand () (Label (TVariable (TypeIndex KType 0)) "min")
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
                            ( EBinaryOperator
                                ()
                                ( TIntrinsic IBool
                                    `TArrow` TIntrinsic IBool
                                    `TArrow` TIntrinsic IBool
                                , OLogicalAnd
                                )
                            )
                            ( EApplication
                                ()
                                (TIntrinsic IBool)
                                ( EVariable
                                    ()
                                    ( Label
                                        ( TVariable (TypeIndex KType 0)
                                            `TArrow` TVariable (TypeIndex KType 0)
                                            `TArrow` TIntrinsic IBool
                                        )
                                        "gt"
                                    )
                                )
                                (EVariable () (Label (TVariable (TypeIndex KType 0)) "n") <| EVariable () (Label (TVariable (TypeIndex KType 0)) "min") :| [])
                                <| EApplication
                                  ()
                                  (TIntrinsic IBool)
                                  ( EBinaryOperator
                                      ()
                                      ( TIntrinsic IBool
                                          `TArrow` TIntrinsic IBool
                                          `TArrow` TIntrinsic IBool
                                      , OLogicalOr
                                      )
                                  )
                                  ( EApplication
                                      ()
                                      (TIntrinsic IBool)
                                      ( EVariable
                                          ()
                                          ( Label
                                              ( TVariable (TypeIndex KType 0)
                                                  `TArrow` TVariable (TypeIndex KType 0)
                                                  `TArrow` TIntrinsic IBool
                                              )
                                              "gt"
                                          )
                                      )
                                      (EVariable () (Label (TVariable (TypeIndex KType 0)) "min") <| EVariable () (Label (TVariable (TypeIndex KType 0)) "max") :| [])
                                      <| EApplication
                                        ()
                                        (TIntrinsic IBool)
                                        ( EVariable
                                            ()
                                            ( Label
                                                ( TVariable (TypeIndex KType 0)
                                                    `TArrow` TVariable (TypeIndex KType 0)
                                                    `TArrow` TIntrinsic IBool
                                                )
                                                "lte"
                                            )
                                        )
                                        (EVariable () (Label (TVariable (TypeIndex KType 0)) "n") <| EVariable () (Label (TVariable (TypeIndex KType 0)) "max") :| [])
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
    ( EVariable
        ()
        ( Label
            ( TIntrinsic
                ( IRecord
                    (TRow (RExtend "max" (TVariable (TypeIndex KType 1)) (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)))
                )
                `TArrow` TVariable (TypeIndex KType 1)
                `TArrow` TIntrinsic IBool
            )
            "in_range"
        )
    )

-- let
--  Some(p) = x     Option(int32)
--  Some(f) = y     Option(int32 -> int32)
--  in
--    f(p)          int32
--
fixture9 :: Expression () (Type TypeIndex ())
fixture9 =
  ( ELet
      ()
      ( BPattern
          ()
          (PConstructor () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "Some") [PVariable () (Label (TIntrinsic IInt32) "p")])
          (EVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "x"))
          <| BPattern
            ()
            (PConstructor () (Label (TApplication () (TConstructor () "Option") ((TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) :| [])) "Some") [PVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f")])
            (EVariable () (Label (TApplication () (TConstructor () "Option") ((TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) :| [])) "y"))
          :| []
      )
      ( EApplication
          ()
          (TIntrinsic IInt32)
          (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f"))
          (EVariable () (Label (TIntrinsic IInt32) "p") :| [])
      )
  )

-- let
--  $v.0 = x                   Option(int32)
--  $v.1 = y                   Option(int32 -> int32)
--  in
--    match(                   int32
--      $v.0                   Option(int32)
--    ) {
--      | Some(p) =>           Option(int32)
--          match(             int32
--            $v.1             Option(int32 -> int32)
--          ) {
--            | Some(f) =>     Option(int32 -> int32)
--                f(p)         int32
--          }
--    }
--
fixture10 :: Expression () (Type TypeIndex ())
fixture10 =
  ( ELet
      ()
      ( BPattern
          ()
          (PVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "$v.0"))
          (EVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "x"))
          <| BPattern
            ()
            (PVariable () (Label (TApplication () (TConstructor () "Option") ((TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) :| [])) "$v.1"))
            (EVariable () (Label (TApplication () (TConstructor () "Option") ((TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) :| [])) "y"))
          :| []
      )
      ( EMatch
          ()
          (TIntrinsic IInt32)
          (EVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "$v.0"))
          ( EClause
              ()
              (PConstructor () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "Some") [PVariable () (Label (TIntrinsic IInt32) "p")])
              ( CPlain
                  ()
                  []
                  ( EMatch
                      ()
                      (TIntrinsic IInt32)
                      (EVariable () (Label (TApplication () (TConstructor () "Option") ((TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) :| [])) "$v.1"))
                      ( EClause
                          ()
                          (PConstructor () (Label (TApplication () (TConstructor () "Option") ((TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) :| [])) "Some") [PVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f")])
                          ( CPlain
                              ()
                              []
                              ( EApplication
                                  ()
                                  (TIntrinsic IInt32)
                                  (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f"))
                                  (EVariable () (Label (TIntrinsic IInt32) "p") :| [])
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

--
-- in_range({ max, min } : Range(a), n : a) =
--   gt(n, min) &&
--     (gt(min, max) || lte(n, max))
--
fixture11 :: Function Expression () (Type TypeIndex Kind)
fixture11 =
  Function
    ()
    (Uses [] (TIntrinsic IBool))
    ( PAnnotation
        ()
        ( TAlias
            "Range"
            [TVariable (Parameter () "a")]
            ( TIntrinsic
                ( IRecord
                    (TRow (RExtend "max" (TVariable (Parameter () "a")) (RExtend "min" (TVariable (Parameter () "a")) RNil)))
                )
            )
        )
        ( PRecord
            ()
            ( TIntrinsic
                ( IRecord
                    (TRow (RExtend "max" (TVariable (TypeIndex KType 0)) (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)))
                )
            )
            ( Map.fromList
                [
                  ( "max"
                  , PShorthand () (Label (TVariable (TypeIndex KType 0)) "max")
                  )
                ,
                  ( "min"
                  , PShorthand () (Label (TVariable (TypeIndex KType 0)) "min")
                  )
                ]
            )
            Nothing
        )
        <| PAnnotation () (TVariable (Parameter () "a")) (PVariable () (Label (TVariable (TypeIndex KType 0)) "n"))
        :| []
    )
    ( EApplication
        ()
        (TIntrinsic IBool)
        ( EBinaryOperator
            ()
            ( TIntrinsic IBool
                `TArrow` TIntrinsic IBool
                `TArrow` TIntrinsic IBool
            , OLogicalAnd
            )
        )
        ( EApplication
            ()
            (TIntrinsic IBool)
            ( EVariable
                ()
                ( Label
                    ( TVariable (TypeIndex KType 0)
                        `TArrow` TVariable (TypeIndex KType 0)
                        `TArrow` TIntrinsic IBool
                    )
                    "gt"
                )
            )
            (EVariable () (Label (TVariable (TypeIndex KType 0)) "n") <| EVariable () (Label (TVariable (TypeIndex KType 0)) "min") :| [])
            <| EApplication
              ()
              (TIntrinsic IBool)
              ( EBinaryOperator
                  ()
                  ( TIntrinsic IBool
                      `TArrow` TIntrinsic IBool
                      `TArrow` TIntrinsic IBool
                  , OLogicalOr
                  )
              )
              ( EApplication
                  ()
                  (TIntrinsic IBool)
                  ( EVariable
                      ()
                      ( Label
                          ( TVariable (TypeIndex KType 0)
                              `TArrow` TVariable (TypeIndex KType 0)
                              `TArrow` TIntrinsic IBool
                          )
                          "gt"
                      )
                  )
                  (EVariable () (Label (TVariable (TypeIndex KType 0)) "min") <| EVariable () (Label (TVariable (TypeIndex KType 0)) "max") :| [])
                  <| EApplication
                    ()
                    (TIntrinsic IBool)
                    ( EVariable
                        ()
                        ( Label
                            ( TVariable (TypeIndex KType 0)
                                `TArrow` TVariable (TypeIndex KType 0)
                                `TArrow` TIntrinsic IBool
                            )
                            "lte"
                        )
                    )
                    (EVariable () (Label (TVariable (TypeIndex KType 0)) "n") <| EVariable () (Label (TVariable (TypeIndex KType 0)) "max") :| [])
                  :| []
              )
            :| []
        )
    )

--
-- in_range($v.0, n : a) =
--   match($v.0) {
--     | { max, min } : Range(a) =>
--         gt(n, min) &&
--           (gt(min, max) || lte(n, max))
--   }
--
fixture12 :: Function Expression () (Type TypeIndex Kind)
fixture12 =
  Function
    ()
    (Uses [] (TIntrinsic IBool))
    ( PVariable
        ()
        ( Label
            ( TIntrinsic
                ( IRecord
                    (TRow (RExtend "max" (TVariable (TypeIndex KType 0)) (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)))
                )
            )
            "$v.0"
        )
        <| PAnnotation () (TVariable (Parameter () "a")) (PVariable () (Label (TVariable (TypeIndex KType 0)) "n"))
        :| []
    )
    ( EMatch
        ()
        (TIntrinsic IBool)
        ( EVariable
            ()
            ( Label
                ( TIntrinsic
                    ( IRecord
                        (TRow (RExtend "max" (TVariable (TypeIndex KType 0)) (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)))
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
                            (TRow (RExtend "max" (TVariable (Parameter () "a")) (RExtend "min" (TVariable (Parameter () "a")) RNil)))
                        )
                    )
                )
                ( PRecord
                    ()
                    ( TIntrinsic
                        ( IRecord
                            (TRow (RExtend "max" (TVariable (TypeIndex KType 0)) (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)))
                        )
                    )
                    ( Map.fromList
                        [
                          ( "max"
                          , PShorthand () (Label (TVariable (TypeIndex KType 0)) "max")
                          )
                        ,
                          ( "min"
                          , PShorthand () (Label (TVariable (TypeIndex KType 0)) "min")
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
                    ( EBinaryOperator
                        ()
                        ( TIntrinsic IBool
                            `TArrow` TIntrinsic IBool
                            `TArrow` TIntrinsic IBool
                        , OLogicalAnd
                        )
                    )
                    ( EApplication
                        ()
                        (TIntrinsic IBool)
                        ( EVariable
                            ()
                            ( Label
                                ( TVariable (TypeIndex KType 0)
                                    `TArrow` TVariable (TypeIndex KType 0)
                                    `TArrow` TIntrinsic IBool
                                )
                                "gt"
                            )
                        )
                        (EVariable () (Label (TVariable (TypeIndex KType 0)) "n") <| EVariable () (Label (TVariable (TypeIndex KType 0)) "min") :| [])
                        <| EApplication
                          ()
                          (TIntrinsic IBool)
                          ( EBinaryOperator
                              ()
                              ( TIntrinsic IBool
                                  `TArrow` TIntrinsic IBool
                                  `TArrow` TIntrinsic IBool
                              , OLogicalOr
                              )
                          )
                          ( EApplication
                              ()
                              (TIntrinsic IBool)
                              ( EVariable
                                  ()
                                  ( Label
                                      ( TVariable (TypeIndex KType 0)
                                          `TArrow` TVariable (TypeIndex KType 0)
                                          `TArrow` TIntrinsic IBool
                                      )
                                      "gt"
                                  )
                              )
                              (EVariable () (Label (TVariable (TypeIndex KType 0)) "min") <| EVariable () (Label (TVariable (TypeIndex KType 0)) "max") :| [])
                              <| EApplication
                                ()
                                (TIntrinsic IBool)
                                ( EVariable
                                    ()
                                    ( Label
                                        ( TVariable (TypeIndex KType 0)
                                            `TArrow` TVariable (TypeIndex KType 0)
                                            `TArrow` TIntrinsic IBool
                                        )
                                        "lte"
                                    )
                                )
                                (EVariable () (Label (TVariable (TypeIndex KType 0)) "n") <| EVariable () (Label (TVariable (TypeIndex KType 0)) "max") :| [])
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
