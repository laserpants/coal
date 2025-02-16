{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LanguageSpec (fixture) where

import Noll.Common.List1 (NonEmpty (..), (<|))
import Noll.Core.Language.Expr (Clause (..), Expr)
import Noll.Core.Language.Syntax (list, opaque, (~>))
import Noll.Label (Label (..))

import qualified Noll.Core.Language.Prim as Core
import qualified Noll.Core.Language.Syntax as Core
import qualified Noll.Core.Language.Op as Core

compareRow :: Core.Type
compareRow = Core.RExt "compare" (opaque ~> opaque ~> Core.TCon "Ordering" []) Core.RNil

-- record({ compare : 0 -> 0 -> Ordering | 0 })
compareDict :: Core.Type
compareDict = Core.record compareRow

ordering :: Core.Type
ordering = Core.TCon "Ordering" []

tree :: Core.Type -> Core.Type
tree t = Core.TCon "Tree" [t]

fixture :: Expr Core.Type
fixture =
  -- let
  --   _compose_ : (0 -> 0) -> (0 -> 0) -> 0 -> 0 =
  --     fn(f : 0 -> 0, g : 0 -> 0, x : 0) =>
  --       @ : 0 (f : 0 -> 0, @ : 0 (g : 0 -> 0, x : 0))
  --     in
  --
  Core.let_
    ( ( Label
          ( (opaque ~> opaque)
              ~> (opaque ~> opaque)
              ~> opaque
              ~> opaque
          )
          "_compose_"
      , Core.lam
          ( Label (opaque ~> opaque) "f"
              :| [ Label (opaque ~> opaque) "g"
                 , Label opaque "x"
                 ]
          )
          ( Core.app
              opaque
              (Core.var (Label (opaque ~> opaque) "f"))
              ( Core.app
                  opaque
                  (Core.var (Label (opaque ~> opaque) "g"))
                  (Core.var (Label opaque "x") :| [])
                  :| []
              )
          )
      )
        :| []
    )
    --       let
    --         _list_concat_ : list(0) -> list(0) -> list(0) =
    --           fn(a : list(0), b : list(0)) =>
    --             match : list(0) (a : list(0)) {
    --               | $Nil : list(0) =>
    --                   b : list(0)
    --               | ($Cons : 0 -> list(0) -> list(0), x : 0, xs : list(0)) =>
    --                   @ : list(0) ($Cons : 0 -> list(0) -> list(0), x : 0
    --                     , @ : list(0) (_list_concat : list(0) -> list(0) -> list(0), xs : list(0), b : list(0)))
    --             }
    --           ;
    --
    ( Core.let_
        ( ( Label (list opaque ~> list opaque ~> list opaque) "_list_concat_"
          , Core.lam
              ( Label (list opaque) "a"
                  :| [Label (list opaque) "b"]
              )
              ( Core.match
                  (list opaque)
                  (Core.var (Label (list opaque) "a"))
                  ( Clause
                      (Label (list opaque) "$Nil" :| [])
                      (Core.var (Label (list opaque) "b"))
                      :| [ Clause
                            ( Label (opaque ~> list opaque ~> list opaque) "$Cons"
                                <| Label opaque "x"
                                <| Label (list opaque) "xs"
                                :| []
                            )
                            ( Core.app
                                (list opaque)
                                (Core.var (Label (opaque ~> list opaque ~> list opaque) "$Cons"))
                                ( Core.var (Label opaque "x")
                                    <| Core.app
                                      (list opaque)
                                      (Core.var (Label (list opaque ~> list opaque ~> list opaque) "_list_concat_"))
                                      ( Core.var (Label (list opaque) "xs")
                                          <| Core.var (Label (list opaque) "b")
                                          :| []
                                      )
                                    :| []
                                )
                            )
                         ]
                  )
              )
          )
            :| [
                 --         compare : record({ compare : 0 -> 0 -> Ordering | 0 }) -> 0 -> 0 -> bool =
                 --           fn(a_1 : record({ compare : 0 -> 0 -> Ordering | 0 }), a_2 : 0, a_3 : 0) =>
                 --             match : bool (a1 : record({ compare : 0 -> 0 -> Ordering | 0 })) {
                 --               | ($Record : { compare : 0 -> 0 -> Ordering | 0 } -> record({ compare : 0 -> 0 -> Ordering | 0 }), r_1 : { compare : 0 -> 0 -> Ordering | 0 }) =>
                 --                   select
                 --                     { compare = f_1 : ? | q_1 : ? } =
                 --                       r_1 : ?
                 --                     in
                 --                       @ : ? (f_1, a_2, a_3)
                 --             }
                 --           ;
                 --

                 ( Label (compareDict ~> opaque ~> opaque ~> Core.bool) "compare"
                 , Core.lam
                    ( Label compareDict "a_1"
                        <| Label opaque "a_2"
                        <| Label opaque "a_3"
                        :| []
                    )
                    ( Core.match
                        Core.bool
                        (Core.var (Label compareDict "a_1"))
                        ( Clause
                            (Label (compareRow ~> compareDict) "$Record" <| Label compareRow "r_1" :| [])
                            undefined
                            :| []
                        )
                    )
                 )
               , --         from_int32 : record({ compare : 0 -> 0 -> Ordering | 0 }) -> int32 -> 0 =
                 --           fn(a_1 : record({ compare : 0 -> 0 -> Ordering | 0 }), a_2 : int32) =>
                 --             match : 0 (a_1 : record({ compare : 0 -> 0 -> Ordering | 0 })) {
                 --               | ($Record, r_1) =>
                 --                   select
                 --                     { from_int32 = f_1 | q_1 } =
                 --                       r_1
                 --                     in
                 --                       @ : ? (f_1, a_2)
                 --             }
                 --           ;
                 --

                 ( Label (compareDict ~> Core.int32 ~> opaque) "from_int32"
                 , Core.lam
                    ( Label compareDict "a_1"
                        <| Label Core.int32 "a_2"
                        :| []
                    )
                    ( Core.match
                        opaque
                        undefined
                        undefined
                    )
                 )
               , --         _forward_application_ : 0 -> (0 -> 0) -> 0 =
                 --           fn(x : 0, f : 0 -> 0) =>
                 --             @ : 0 (f : 0 -> 0, x : 0)
                 --           ;
                 --

                 ( Label (opaque ~> (opaque ~> opaque) ~> opaque) "_forward_application_"
                 , Core.lam
                    (Label opaque "x" <| Label (opaque ~> opaque) "f" :| [])
                    ( Core.app
                        opaque
                        (Core.var (Label (opaque ~> opaque) "f"))
                        (Core.var (Label opaque "x") :| [])
                    )
                 )
               , --         _not_ : bool -> bool =
                 --           fn(a) =>
                 --             if a then false else true
                 --           ;
                 --

                 ( Label (Core.bool ~> Core.bool) "_not_"
                 , Core.lam
                    (Label Core.bool "a" :| [])
                    ( Core.if_
                        (Core.var (Label Core.bool "a"))
                        (Core.lit (Core.PBool False))
                        (Core.lit (Core.PBool True))
                    )
                 )
               , --         compare__int32 : int32 -> int32 -> Ordering =
                 --           fn(x : int32, y : int32) =>
                 --             if (x : int32 [< int32] y : int32)
                 --               then
                 --                 LessThan : Ordering
                 --               else
                 --                 if (x [> int32] y)
                 --                   then
                 --                     GreaterThan : Ordering
                 --                   else
                 --                     EqualTo : Ordering
                 --           ;
                 --

                 ( Label (Core.int32 ~> Core.int32 ~> ordering) "compare__int32"
                 , Core.lam
                    ( Label Core.int32 "x"
                        <| Label Core.int32 "y"
                        :| []
                    )
                    ( Core.if_
                        (
                          Core.op
                            (Core.OLtInt32
                              (Core.var (Label Core.int32 "x"))
                              (Core.var (Label Core.int32 "y"))
                            )
                        )
                        (Core.var (Label ordering "LessThan"))
                        ( Core.if_
                            (
                              Core.op
                                (Core.OGtInt32
                                  (Core.var (Label Core.int32 "x"))
                                  (Core.var (Label Core.int32 "y"))
                                )
                            )
                            (Core.var (Label ordering "GreaterThan"))
                            (Core.var (Label ordering "EqualTo"))
                        )
                    )
                 )
               , --         from_int32__int32 : int32 -> int32 =
                 --           fn(n : int32) =>
                 --             n : int32
                 --           ;
                 --

                 ( Label undefined "from_int32__int32"
                 , Core.lam
                    undefined
                    undefined
                 )
               , --         lte : record({ compare : 0 -> 0 -> Ordering | 0 }) -> 0 -> 0 -> bool =
                 --           fn(d_1 : record({ compare : 0 -> 0 -> Ordering | 0 })) =>
                 --             fn(x : 0) =>
                 --               fn(y : 0) =>
                 --                 match : bool (@ : Ordering (compare : record({ compare : 0 -> 0 -> Ordering | 0 }) -> 0 -> 0 -> Ordering, d_1 : record({ compare : 0 -> 0 -> Ordering | 0 }), x : 0, y : 0)) {
                 --                   | LessThan : Ordering =>
                 --                       true
                 --                   | EqualTo : Ordering =>
                 --                       true
                 --                   | GreaterThan : Ordering =>
                 --                       false
                 --                 }
                 --           ;
                 --

                 ( Label undefined "lte"
                 , Core.lam
                    undefined
                    undefined
                 )
               , --         gt : record({ compare : 0 -> 0 -> Ordering | 0 }) -> 0 -> 0 -> bool =
                 --           fn(d_1 : record({ compare : 0 -> 0 -> Ordering | 0 })) =>
                 --             fn(x : 0) =>
                 --               @ : 0 -> bool ( _compose_ : (bool -> bool) -> (0 -> bool) -> 0 -> bool
                 --                             , _not_ : bool -> bool
                 --                             , @ : 0 -> bool ( lte : record({ compare : 0 -> 0 -> Ordering | 0 }) -> 0 -> 0 -> bool
                 --                                             , d_1 : record({ compare : 0 -> 0 -> Ordering | 0 })
                 --                                             , x : 0))
                 --           ;
                 --

                 ( Label (compareDict ~> opaque ~> opaque ~> Core.bool) "gt"
                 , Core.lam
                    (Label Core.bool "d_1" :| [])
                    ( Core.lam
                        (Label Core.bool "x" :| [])
                        ( Core.app
                            undefined
                            (Core.var (Label undefined "_compose_"))
                            ( Core.var (Label undefined "_not_")
                                :| [ Core.app
                                      undefined
                                      (Core.var (Label undefined "lte"))
                                      ( Core.var (Label undefined "d_1")
                                          :| [ Core.var (Label undefined "x")
                                             ]
                                      )
                                   ]
                            )
                        )
                    )
                 )
               , --         in_range : ? =
                 --           fn(d_1) =>
                 --             fn(range, n) =>
                 --               match(range) {
                 --                 ($Record, row_1) =>
                 --                   select
                 --                     { min = min | row_2 } =
                 --                       row_1
                 --                     in
                 --                       select
                 --                         { max = max | z } =
                 --                           row_2
                 --                         in
                 --                           ?
                 --               }
                 --           ;
                 --

                 ( Label undefined "in_range"
                 , Core.lam
                    undefined
                    undefined
                 )
               , --         from_list : record({ compare : 0 -> 0 -> Ordering | 0 }) -> list(0) -> tree(0) =
                 --

                 ( Label undefined "from_list"
                 , Core.lam
                    undefined
                    undefined
                 )
               , --         flatten : Tree(0) -> list(0) =
                 --

                 ( Label (tree opaque ~> list opaque) "flatten"
                 , Core.lam
                    undefined
                    undefined
                 )
               , --         qsort : ? =
                 --

                 ( Label undefined "qsort"
                 , Core.lam
                    undefined
                    undefined
                 )
               ]
        )
        ( Core.let_
            ( ( Label undefined "xs"
              , undefined
              )
                :| []
            )
            ( Core.let_
                ( ( Label undefined "ys"
                  , undefined
                  )
                    :| []
                )
                undefined
            )
        )
    )
