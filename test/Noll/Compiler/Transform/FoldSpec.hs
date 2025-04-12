{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Transform.FoldSpec where

import Lang.Common.List1 (List1 (..), NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Compiler.Transform.Fold
import Noll.Examples.Test02 (test02)
import Noll.Examples.Test03 (test03)
import Noll.Language (BinaryOperator (..), Binding (..), Choice (..), Clause (..), Expression (..), Pattern (..), Primitive (..))
import Test.Hspec (Spec, describe, it)

import qualified Data.Map.Strict as Map
import qualified Noll.Examples.Test02 as Test02
import qualified Noll.Examples.Test03 as Test03
import qualified Noll.Set.Test02
import qualified Noll.Set.Test03

spec :: Spec
spec =
  describe "Noll.Compiler.Transform.Fold" $ do
    describe "" $ do
      it "" $ do
        runFoldExpansion "fold" 1 (expandFoldExpr exprs1 clauses1) == result1
      it "" $ do
        runFoldExpansion "fold" 1 (compileFolds Test02.moduleBinarySearch) == Test03.moduleBinarySearch
      it "" $ do
        runFoldExpansion "fold" 1 (compileFolds test02) == test03
      it "" $ do
        runFoldExpansion "fold" 1 (compileFolds Noll.Set.Test02.prog1_02) == Noll.Set.Test03.prog1_03

--
-- list, { min = 0, max = -1 }
--
exprs1 :: List1 (Expression () ())
exprs1 =
  EVariable () (Label () "list")
    <| ERecord
      ()
      ()
      ( Map.fromList
          [
            ( "min"
            , ELiteral () (LInt32 0)
            )
          ,
            ( "max"
            , ELiteral () (LInt32 (-1))
            )
          ]
      )
      Nothing
    :| []

--
-- p :: @g => fn(range) => if p |.in_range(range) then Node(p, g({ min = range.min, max = p}), g({ min = p, max = range.max })) else g(range)
-- [] => fn(_) => Leaf
--
clauses1 :: List1 (Clause () ())
clauses1 =
  EClause
    ()
    ( PListCons
        ()
        ()
        (PVariable () (Label () "p"))
        (PAtVariable () (Label () "g"))
    )
    ( CPlain
        ()
        []
        ( ELambda
            ()
            (PVariable () (Label () "range") :| [])
            ( EIf
                ()
                ()
                ( EApplication
                    ()
                    ()
                    (EBinaryOperator () () OForwardApplication)
                    ( EVariable () (Label () "p")
                        <| EApplication
                          ()
                          ()
                          (EVariable () (Label () "in_range"))
                          (EVariable () (Label () "range") :| [])
                        :| []
                    )
                )
                ( EApplication
                    ()
                    ()
                    (EConstructor () (Label () "Node"))
                    ( EVariable () (Label () "p")
                        <| EApplication
                          ()
                          ()
                          (EVariable () (Label () "g"))
                          ( ERecord
                              ()
                              ()
                              ( Map.fromList
                                  [
                                    ( "min"
                                    , ESelect
                                        ()
                                        (Label () "min")
                                        (EVariable () (Label () "range"))
                                    )
                                  ,
                                    ( "max"
                                    , EVariable () (Label () "p")
                                    )
                                  ]
                              )
                              Nothing
                              :| []
                          )
                        <| EApplication
                          ()
                          ()
                          (EVariable () (Label () "g"))
                          ( ERecord
                              ()
                              ()
                              ( Map.fromList
                                  [
                                    ( "min"
                                    , EVariable () (Label () "p")
                                    )
                                  ,
                                    ( "max"
                                    , ESelect
                                        ()
                                        (Label () "max")
                                        (EVariable () (Label () "range"))
                                    )
                                  ]
                              )
                              Nothing
                              :| []
                          )
                        :| []
                    )
                )
                ( EApplication
                    ()
                    ()
                    (EVariable () (Label () "g"))
                    (EVariable () (Label () "range") :| [])
                )
            )
        )
        :| []
    )
    <| EClause
      ()
      (PListLiteral () () [])
      ( CPlain
          ()
          []
          ( ELambda
              ()
              (PAny () () :| [])
              (EConstructor () (Label () "Leaf"))
          )
          :| []
      )
    :| []

--
-- let
--   $fold.1 =
--     fn($fold.1.expr) =>
--       match($fold.1.expr) {
--         | p :: g =>
--             fn(range) =>
--               if p |.in_range(range)
--                 then Node(p, $fold.1(g, { min = range.min, max = p}), $fold.1(g, { min = p, max = range.max }))
--                 else g(range)
--         | [] =>
--             fn(_) =>
--               Leaf
--       }
--   in
--     $fold.1(list, { min = 0, max = -1 })
result1 :: Expression () ()
result1 =
  ERecursiveLet
    ()
    (PVariable () (Label () "$fold.1"))
    ( ELambda
        ()
        (PVariable () (Label () "$fold.1.expr") :| [])
        ( EMatch
            ()
            ()
            (EVariable () (Label () "$fold.1.expr"))
            ( EClause
                ()
                ( PListCons
                    ()
                    ()
                    (PVariable () (Label () "p"))
                    (PVariable () (Label () "g"))
                )
                ( CPlain
                    ()
                    []
                    ( ELambda
                        ()
                        (PVariable () (Label () "range") :| [])
                        ( EIf
                            ()
                            ()
                            ( EApplication
                                ()
                                ()
                                (EBinaryOperator () () OForwardApplication)
                                ( EVariable () (Label () "p")
                                    <| EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "in_range"))
                                      (EVariable () (Label () "range") :| [])
                                    :| []
                                )
                            )
                            ( EApplication
                                ()
                                ()
                                (EConstructor () (Label () "Node"))
                                ( EVariable () (Label () "p")
                                    <| EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "$fold.1"))
                                      ( EVariable () (Label () "g")
                                          <| ERecord
                                            ()
                                            ()
                                            ( Map.fromList
                                                [
                                                  ( "min"
                                                  , ESelect
                                                      ()
                                                      (Label () "min")
                                                      (EVariable () (Label () "range"))
                                                  )
                                                ,
                                                  ( "max"
                                                  , EVariable () (Label () "p")
                                                  )
                                                ]
                                            )
                                            Nothing
                                          :| []
                                      )
                                    <| EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "$fold.1"))
                                      ( EVariable () (Label () "g")
                                          <| ERecord
                                            ()
                                            ()
                                            ( Map.fromList
                                                [
                                                  ( "min"
                                                  , EVariable () (Label () "p")
                                                  )
                                                ,
                                                  ( "max"
                                                  , ESelect
                                                      ()
                                                      (Label () "max")
                                                      (EVariable () (Label () "range"))
                                                  )
                                                ]
                                            )
                                            Nothing
                                          :| []
                                      )
                                    :| []
                                )
                            )
                            ( EApplication
                                ()
                                ()
                                (EVariable () (Label () "$fold.1"))
                                ( EVariable () (Label () "g")
                                    <| EVariable () (Label () "range")
                                    :| []
                                )
                            )
                        )
                    )
                    :| []
                )
                <| EClause
                  ()
                  (PListLiteral () () [])
                  ( CPlain
                      ()
                      []
                      ( ELambda
                          ()
                          (PAny () () :| [])
                          ( EConstructor () (Label () "Leaf")
                          )
                      )
                      :| []
                  )
                :| []
            )
        )
    )
    ( EApplication
        ()
        ()
        (EVariable () (Label () "$fold.1"))
        ( EVariable () (Label () "list")
            <| ERecord
              ()
              ()
              ( Map.fromList
                  [
                    ( "min"
                    , ELiteral () (LInt32 0)
                    )
                  ,
                    ( "max"
                    , ELiteral () (LInt32 (-1))
                    )
                  ]
              )
              Nothing
            :| []
        )
    )
