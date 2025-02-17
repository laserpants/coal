{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LanguageSpec (fixture) where

import Noll.Common.List1 (NonEmpty (..), (<|))
import Noll.Core.Language.Expr (Clause (..), Expr, Focus (..))
import Noll.Core.Language.Syntax (list, opaque, (~>))
import Noll.Label (Label (..))

import qualified Noll.Core.Language.Op as Core
import qualified Noll.Core.Language.Prim as Core
import qualified Noll.Core.Language.Syntax as Core

compareRow :: Core.Type
compareRow = Core.RExt "compare" (opaque ~> opaque ~> Core.TCon "Ordering" []) Core.RNil

fromInt32Row :: Core.Type
fromInt32Row = Core.RExt "from_int32" (Core.int32 ~> opaque) Core.RNil

-- record({ compare : 0 -> 0 -> Ordering | 0 })
compareDict :: Core.Type
compareDict = Core.record compareRow

-- record({ from_int32 : int32 -> 0 | 0 })
fromInt32Dict :: Core.Type
fromInt32Dict = Core.record fromInt32Row

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
    ( ( Label ((opaque ~> opaque) ~> (opaque ~> opaque) ~> opaque ~> opaque) "_compose_"
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
              (Label (list opaque) "a" :| [Label (list opaque) "b"])
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
                 --         compare : record({ compare : 0 -> 0 -> Ordering | 0 }) -> 0 -> 0 -> Ordering =
                 --           fn(a_1 : record({ compare : 0 -> 0 -> Ordering | 0 }), a_2 : 0, a_3 : 0) =>
                 --             match : Ordering (a1 : record({ compare : 0 -> 0 -> Ordering | 0 })) {
                 --               | ($Record : { compare : 0 -> 0 -> Ordering | 0 } -> record({ compare : 0 -> 0 -> Ordering | 0 }), r_1 : { compare : 0 -> 0 -> Ordering | 0 }) =>
                 --                   select
                 --                     { compare = f_1 : 0 -> 0 -> Ordering | q_1 : 0 } =
                 --                       r_1 : { compare : 0 -> 0 -> Ordering | 0 }
                 --                     in
                 --                       @ : Ordering (f_1 : 0 -> 0 -> Ordering, a_2 : 0, a_3 : 0)
                 --             }
                 --           ;
                 --

                 ( Label (compareDict ~> opaque ~> opaque ~> ordering) "compare"
                 , Core.lam
                    ( Label compareDict "a_1"
                        <| Label opaque "a_2"
                        <| Label opaque "a_3"
                        :| []
                    )
                    ( Core.match
                        ordering
                        (Core.var (Label compareDict "a_1"))
                        ( Clause
                            (Label (compareRow ~> compareDict) "$Record" <| Label compareRow "r_1" :| [])
                            ( Core.sel
                                ( Focus
                                    "compare"
                                    (Label (opaque ~> opaque ~> ordering) "f_1")
                                    (Label opaque "q_1")
                                )
                                (Core.var (Label compareRow "r_1"))
                                ( Core.app
                                    ordering
                                    (Core.var (Label (opaque ~> opaque ~> ordering) "f_1"))
                                    ( Core.var (Label opaque "a_2")
                                        <| Core.var (Label opaque "a_3")
                                        :| []
                                    )
                                )
                            )
                            :| []
                        )
                    )
                 )
               , --         from_int32 : record({ from_int32 : int32 -> 0 | 0 }) -> int32 -> 0 =
                 --           fn(a_1 : record({ from_int32 : int32 -> 0 | 0 }), a_2 : int32) =>
                 --             match : 0 (a_1 : record({ from_int32 : int32 -> 0 | 0 })) {
                 --               | ($Record : { from_int32 : int32 -> 0 | 0 } -> record({ from_int32 : int32 -> 0 | 0 }), r_1 : { from_int32 : int32 -> 0 | 0 }) =>
                 --                   select
                 --                     { from_int32 = f_1 : int32 -> 0 | q_1 : 0 } =
                 --                       r_1 : { from_int32 : int32 -> 0 | 0 }
                 --                     in
                 --                       @ : 0 (f_1 : int32 -> 0, a_2 : int32)
                 --             }
                 --           ;
                 --

                 ( Label (fromInt32Dict ~> Core.int32 ~> opaque) "from_int32"
                 , Core.lam
                    ( Label fromInt32Dict "a_1"
                        <| Label Core.int32 "a_2"
                        :| []
                    )
                    ( Core.match
                        opaque
                        (Core.var (Label fromInt32Dict "a_1"))
                        ( Clause
                            (Label (fromInt32Row ~> fromInt32Dict) "$Record" <| Label fromInt32Row "r_1" :| [])
                            ( Core.sel
                                ( Focus
                                    "from_int32"
                                    (Label (Core.int32 ~> opaque) "f_1")
                                    (Label opaque "q_1")
                                )
                                (Core.var (Label fromInt32Row "r_1"))
                                ( Core.app
                                    opaque
                                    (Core.var (Label (Core.int32 ~> opaque) "f_1"))
                                    (Core.var (Label Core.int32 "a_2") :| [])
                                )
                            )
                            :| []
                        )
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
                 --           fn(a : bool) =>
                 --             if (a : bool) then false else true
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
                 --                 if (x : int32 [> int32] y : int32)
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
                        ( Core.op
                            ( Core.OLtInt32
                                (Core.var (Label Core.int32 "x"))
                                (Core.var (Label Core.int32 "y"))
                            )
                        )
                        (Core.var (Label ordering "LessThan"))
                        ( Core.if_
                            ( Core.op
                                ( Core.OGtInt32
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

                 ( Label (Core.int32 ~> Core.int32) "from_int32__int32"
                 , Core.lam
                    (Label Core.int32 "n" :| [])
                    (Core.var (Label Core.int32 "n"))
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

                 ( Label (compareDict ~> opaque ~> opaque ~> Core.bool) "lte"
                 , Core.lam
                    (Label compareDict "d_1" :| [])
                    ( Core.lam
                        (Label opaque "x" :| [])
                        ( Core.lam
                            (Label opaque "y" :| [])
                            ( Core.match
                                Core.bool
                                ( Core.app
                                    ordering
                                    (Core.var (Label (compareDict ~> opaque ~> opaque ~> ordering) "compare"))
                                    ( Core.var (Label compareDict "d_1")
                                        <| Core.var (Label opaque "x")
                                        <| Core.var (Label opaque "y")
                                        :| []
                                    )
                                )
                                ( Clause
                                    (Label ordering "LessThan" :| [])
                                    (Core.lit (Core.PBool True))
                                    <| Clause
                                      (Label ordering "EqualTo" :| [])
                                      (Core.lit (Core.PBool True))
                                    <| Clause
                                      (Label ordering "GreaterThan" :| [])
                                      (Core.lit (Core.PBool False))
                                    :| []
                                )
                            )
                        )
                    )
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
                    (Label compareDict "d_1" :| [])
                    ( Core.lam
                        (Label opaque "x" :| [])
                        ( Core.app
                            (opaque ~> Core.bool)
                            (Core.var (Label ((Core.bool ~> Core.bool) ~> (opaque ~> Core.bool) ~> opaque ~> Core.bool) "_compose_"))
                            ( Core.var (Label (Core.bool ~> Core.bool) "_not_")
                                :| [ Core.app
                                      (opaque ~> Core.bool)
                                      (Core.var (Label (compareDict ~> opaque ~> opaque ~> Core.bool) "lte"))
                                      ( Core.var (Label compareDict "d_1")
                                          :| [ Core.var (Label opaque "x")
                                             ]
                                      )
                                   ]
                            )
                        )
                    )
                 )
               , -- ///////////////////////////////////////////////////////////
                 -- ///////////////////////////////////////////////////////////
                 -- ///////////////////////////////////////////////////////////
                 --
                 --         in_range : record({ compare : 0 -> 0 -> Ordering | 0 }) -> ? =
                 --           fn(d_1 : record({ compare : 0 -> 0 -> Ordering | 0 })) =>
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
                    (Label undefined "d_1" :| [])
                    ( Core.lam
                        ( Label undefined "range"
                            <| Label undefined "n"
                            :| []
                        )
                        ( Core.match
                            undefined
                            undefined
                            undefined
                        )
                    )
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
                    (Label undefined "tree" :| [])
                    ( Core.let_
                        undefined
                        undefined
                    )
                 )
               , --         qsort : record({ compare : 0 -> 0 -> Ordering | 0 }) -> list(0) -> list(0) =
                 --           fn(d_1 : record({ compare : 0 -> 0 -> Ordering | 0 })) =>
                 --             ?
                 --

                 ( Label (compareDict ~> list opaque ~> list opaque) "qsort"
                 , Core.lam
                    (Label compareDict "d_1" :| [])
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
