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
compareRow = Core.RExt "compare" (opaque ~> opaque ~> Core.TCon "Ordering" []) opaque

fromInt32Row :: Core.Type
fromInt32Row = Core.RExt "from_int32" (Core.int32 ~> opaque) opaque

maxMinRow :: Core.Type -> Core.Type
maxMinRow r = Core.RExt "max" opaque (Core.RExt "min" opaque r)

-- record({ compare : 0 -> 0 -> Ordering | 0 })
compareDict :: Core.Type
compareDict = Core.record compareRow

-- record({ from_int32 : int32 -> 0 | 0 })
fromInt32Dict :: Core.Type
fromInt32Dict = Core.record fromInt32Row

ordering :: Core.Type
ordering = Core.TCon "Ordering" []

-- record({ max : 0 | min : 0 | r })
maxMinRecord :: Core.Type -> Core.Type
maxMinRecord r = Core.record (maxMinRow r)

tree :: Core.Type -> Core.Type
tree t = Core.TCon "Tree" [t]

fixture :: Expr Core.Type
fixture =
  --
  -- let
  --   _compose_ : (0 -> 0) -> (0 -> 0) -> 0 -> 0 =
  --
  Core.let_
    ( ( Label ((opaque ~> opaque) ~> (opaque ~> opaque) ~> opaque ~> opaque) "_compose_"
      , --
        --     fn( f : 0 -> 0
        --       , g : 0 -> 0
        --       , x : 0 ) =>
        --
        Core.lam
          ( Label (opaque ~> opaque) "f"
              :| [ Label (opaque ~> opaque) "g"
                 , Label opaque "x"
                 ]
          )
          --
          --       @ : 0 ( f : 0 -> 0
          --             , @ : 0 ( g : 0 -> 0
          --                     , x : 0 ))
          --
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
    --
    --     in
    --       let
    --         _list_concat_ : list(0) -> list(0) -> list(0) =
    --
    ( Core.let_
        ( ( Label (list opaque ~> list opaque ~> list opaque) "_list_concat_"
          , --
            --           fn(a : list(0), b : list(0)) =>
            --
            Core.lam
              (Label (list opaque) "a" :| [Label (list opaque) "b"])
              --
              --             match : list(0) (a : list(0)) {
              --
              ( Core.match
                  (list opaque)
                  (Core.var (Label (list opaque) "a"))
                  --
                  --               | $Nil : list(0) =>
                  --                   b : list(0)
                  --
                  ( Clause
                      (Label (list opaque) "$Nil" :| [])
                      (Core.var (Label (list opaque) "b"))
                      --
                      --               | ($Cons : 0 -> list(0) -> list(0), x : 0, xs : list(0)) =>
                      --
                      :| [ Clause
                            ( Label (opaque ~> list opaque ~> list opaque) "$Cons"
                                <| Label opaque "x"
                                <| Label (list opaque) "xs"
                                :| []
                            )
                            --
                            --                   @ : list(0) ( $Cons : 0 -> list(0) -> list(0)
                            --                               , x : 0
                            --                               , @ : list(0) ( _list_concat : list(0) -> list(0) -> list(0)
                            --                                             , xs : list(0)
                            --                                             , b : list(0)))
                            --
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
                         --
                         --             }
                         --           ;
                         --
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
               , --
                 --         in_range : record({ compare : 0 -> 0 -> Ordering | 0 }) -> record({ max : 0 | min : 0 | 0 }) -> 0 -> bool =
                 --

                 ( Label (compareDict ~> maxMinRecord opaque ~> opaque ~> Core.bool) "in_range"
                 , --
                   --           fn(d_1 : record({ compare : 0 -> 0 -> Ordering | 0 })) =>
                   --
                   Core.lam
                    (Label compareDict "d_1" :| [])
                    --
                    --             fn(range : record({ max : 0 | min : 0 | 0 }), n : 0) =>
                    --
                    ( Core.lam
                        (Label (maxMinRecord opaque) "range" <| Label opaque "n" :| [])
                        --
                        --               match : bool (range : record({ max : 0 | min : 0 | 0 })) {
                        --
                        ( Core.match
                            Core.bool
                            (Core.var (Label (maxMinRecord opaque) "range"))
                            ( --
                              --                 ( $Record : { max : 0 | min : 0 | 0 } -> record({ max : 0 | min : 0 | 0 })
                              --                 , row_1 : { max : 0 | min : 0 | 0 } 
                              --                 ) =>
                              --                 
                              Clause
                                ( Label (maxMinRow opaque ~> maxMinRecord opaque) "$Record"
                                    <| Label (maxMinRow opaque) "row_1"
                                    :| []
                                )
                                --                   select
                                --                     { min = min : 0 | row_2 : { max : 0 | 0 } } =
                                --                       row_1 : { max : 0 | min : 0 | 0 }
                                ( Core.sel
                                    ( Focus
                                        undefined
                                        undefined
                                        undefined
                                    )
                                    undefined
                            --                     in
                            --                       select
                            --                         { max = max : 0 | z : 0 } =
                            --                           row_2 : { max : 0 | 0 }
                                    (
                                      Core.sel
                                        ( Focus
                                            undefined
                                            undefined
                                            undefined
                                        )
                                        undefined
                                        undefined
                                    )
                                )
                                :| []
                            )
                            --                         in
                            --                           @ : bool ( gt : record({ compare : 0 -> 0 -> Ordering | 0 }) -> 0 -> 0 -> bool
                            --                                    , d_1 : record({ compare : 0 -> 0 -> Ordering | 0 })
                            --                                    , n : 0
                            --                                    , min : 0 )
                            --                           &&
                            --                           (
                            --                             @ : bool ( gt : record({ compare : 0 -> 0 -> Ordering | 0 }) -> 0 -> 0 -> bool
                            --                                      , d_1 : record({ compare : 0 -> 0 -> Ordering | 0 })
                            --                                      , min : 0
                            --                                      , max : 0 )
                            --                             ||
                            --                             @ : bool ( lte : record({ compare : 0 -> 0 -> Ordering | 0 }) -> 0 -> 0 -> bool
                            --                                      , d_1 : record({ compare : 0 -> 0 -> Ordering | 0 })
                            --                                      , n : 0
                            --                                      , max : 0 )
                            --                           )
                            --               }
                            --           ;
                            --
                        )
                    )
                 )
               , --         from_list : record({ compare : 0 -> 0 -> Ordering | 0 }) -> list(0) -> Tree(0) =
                 --           fn(d_1 : record({ compare : 0 -> 0 -> Ordering | 0 })) =>
                 --             fn(list : list(0)) =>
                 --               let
                 --                 fold_ : list(0) -> record({ max : 0 | min : 0 | 0 }) -> Tree(0) =
                 --                   fn(a_0 : list(0)) =>
                 --                     match(a_0 : list(0)) {
                 --                       | ($cons : 0 -> list(0) -> list(0), p : 0, g : list(0)) =>
                 --                           fn(range : record({ max : 0 | min : 0 | 0 })) =>
                 --                             if ( @ : bool
                 --                                  ( _forward_application_ : 0 -> (0 -> bool) -> bool
                 --                                  , p : 0
                 --                                  , @ : 0 -> bool
                 --                                      ( in_range : record({ compare : 0 -> 0 -> Ordering | 0 }) -> record({ max : 0 | min : 0 | 0 }) -> 0 -> bool
                 --                                      , d_1 : record({ compare : 0 -> 0 -> Ordering | 0 })
                 --                                      , range : record({ max : 0 | min : 0 | 0 })
                 --                                      )
                 --                                  )
                 --                                )
                 --                               then
                 --                                 @ : Tree(0)
                 --                                   ( Node : 0 -> Tree(0) -> Tree(0) -> Tree(0)
                 --                                   , p : 0
                 --                                   , @ : Tree(0)
                 --                                       ( fold_ : list(0) -> record({ max : 0 | min : 0 | {} }) -> Tree(0)
                 --                                       , g : list(0)
                 --                                       , @ : record({ max : 0 | min : 0 | {} })
                 --                                         ( $Record : ?
                 --                                         , { min =
                 --                                               match(range : record({ max : 0 | min : 0 | 0 })) {
                 --                                                 |
                 --                                               }
                 --                                           | max = p : 0
                 --                                           | {}
                 --                                           }
                 --                                         )
                 --                                       )
                 --                                   , @ : Tree(0)
                 --                                       ( fold_ : list(0) -> record({ max : 0 | min : 0 | {} }) -> Tree(0)
                 --                                       , g : list(0)
                 --                                       , @ : record({ max : 0 | min : 0 | {} })
                 --                                         ( $Record : ?
                 --                                         , { min = p : 0
                 --                                           | max =
                 --                                               match(range : record({ max : 0 | min : 0 | 0 })) {
                 --                                                 |
                 --                                               }
                 --                                           | {}
                 --                                           }
                 --                                         )
                 --                                       )
                 --                                   )
                 --                               else
                 --                                 @ : ?
                 --                                   ( fold_ : list(0) -> record({ max : 0 | min : 0 | 0 }) -> Tree(0)
                 --                                   , g : list(0)
                 --                                   , range : record({ max : 0 | min : 0 | 0 })
                 --                                   )
                 --                       | $Nil : list(0) =>
                 --                           fn(_ : record({ max : 0 | min : 0 | 0 })) =>
                 --                             Leaf : Tree(0)
                 --                     }
                 --                 in
                 --                   @ : Tree(0)
                 --                     ( fold_ : list(0) -> record({ max : 0 | min : 0 | {} }) -> Tree(0)
                 --                     , list : list(0)
                 --                     , @ : record({ max : 0 | min : 0 | {} })
                 --                       ( $Record : record({ max : 0 | min : 0 | {} })
                 --                       , { min = @ : 0 (from_int32 : int32 -> 0, d_1 : record({ compare : 0 -> 0 -> Ordering | 0 }), 0 : int32)
                 --                         , max = @ : 0 (from_int32 : int32 -> 0, d_1 : record({ compare : 0 -> 0 -> Ordering | 0 }), -1 : int32)
                 --                         , {}
                 --                         }
                 --                       )
                 --                     )
                 --           ;
                 --

                 ( Label (compareDict ~> list opaque ~> tree opaque) "from_list"
                 , Core.lam
                    undefined
                    undefined
                 )
               , --         flatten : Tree(0) -> list(0) =
                 --           fn(tree : Tree(0)) =>
                 --             let
                 --               fold_ : Tree(0) -> list(0) =
                 --                 fn(a_0 : Tree(0)) =>
                 --                   match(a_0 : Tree(0)) {
                 --                     | (Node : 0 -> Tree(0) -> Tree(0) -> Tree(0), y : 0, lhs : Tree(0), rhs : Tree(0)) =>
                 --                         @ : list(0)
                 --                           ( _list_concat_ : list(0) -> list(0) -> list(0)
                 --                           , @ : list(0)
                 --                               ( fold_ : Tree(0) -> list(0)
                 --                               , lhs : Tree(0))
                 --                           , @ : list(0)
                 --                               ( $Cons : 0 -> list(0) -> list(0)
                 --                               , y : 0
                 --                               , @ : list(0)
                 --                                 ( fold_ : Tree(0) -> list(0)
                 --                                 , rhs : Tree(0))
                 --                               )
                 --                           )
                 --                     | Leaf : Tree(0) =>
                 --                         $Nil : list(0)
                 --                   }
                 --               in
                 --                 @ : list(0)
                 --                   ( fold_ : Tree(0) -> list(0)
                 --                   , tree : Tree(0))
                 --           ;

                 ( Label (tree opaque ~> list opaque) "flatten"
                 , Core.lam
                    (Label (tree opaque) "tree" :| [])
                    ( Core.let_
                        ( ( Label undefined "fold_"
                          , Core.lam
                              (Label undefined "a_0" :| [])
                              undefined
                          )
                            :| []
                        )
                        ( Core.app
                            undefined
                            (Core.var (Label undefined "fold_"))
                            (Core.var (Label undefined "tree") :| [])
                        )
                    )
                 )
               , --         qsort : record({ compare : 0 -> 0 -> Ordering | 0 }) -> list(0) -> list(0) =
                 --           fn(d_1 : record({ compare : 0 -> 0 -> Ordering | 0 })) =>
                 --             @ : list(0) -> list(0) ( _compose_ : (Tree(0) -> list(0)) -> (list(0) -> Tree(0)) -> list(0) -> list(0)
                 --                                    , flatten : Tree(0) -> list(0)
                 --                                    , @ : list(0) -> Tree(0) ( from_list : record({ compare : 0 -> 0 -> Ordering | 0 }) -> list(0) -> Tree(0)
                 --                                                             , d_1 : record({ compare : 0 -> 0 -> Ordering | 0 })
                 --                                                             ))
                 --           ;

                 ( Label (compareDict ~> list opaque ~> list opaque) "qsort"
                 , Core.lam
                    (Label compareDict "d_1" :| [])
                    ( Core.app
                        (list opaque ~> list opaque)
                        ( Core.var
                            ( Label
                                ( (tree opaque ~> list opaque)
                                    ~> (list opaque ~> tree opaque)
                                    ~> list opaque
                                    ~> list opaque
                                )
                                "_compose_"
                            )
                        )
                        ( Core.var (Label (tree opaque ~> list opaque) "flatten")
                            <| Core.app
                              (list opaque ~> tree opaque)
                              (Core.var (Label (compareDict ~> list opaque ~> tree opaque) "from_list"))
                              (Core.var (Label compareDict "d_1") :| [])
                            :| []
                        )
                    )
                 )
               ]
        )
        --
        -- in
        --   let
        --     xs =
        --       @ : list(int32)
        --         ( $Cons
        --         , 2 : int32
        --         , @ : list(int32)
        --             ( $Cons
        --             , 105 : int32
        --             , @ : list(int32)
        --                 ( $Cons
        --                 , 103 : int32
        --                 , @ : list(int32)
        --                   ( $Cons
        --                   , 104 : int32
        --                   , @ : list(int32)
        --                     ( $Cons
        --                     , 2 : int32
        --                     , @ : list(int32)
        --                       ( $Cons
        --                       , 106 : int32
        --                       , $Nil : list(int32)
        --                       ))))))
        --
        ( Core.let_
            ( ( Label undefined "xs"
              , Core.app
                  undefined
                  undefined
                  ( Core.app
                      undefined
                      undefined
                      ( Core.app
                          undefined
                          undefined
                          ( Core.app
                              undefined
                              undefined
                              ( Core.app
                                  undefined
                                  undefined
                                  ( Core.app
                                      undefined
                                      undefined
                                      undefined
                                      :| []
                                  )
                                  :| []
                              )
                              :| []
                          )
                          :| []
                      )
                      :| []
                  )
              )
                :| []
            )
            --
            -- in
            --   let
            --     ys =
            --       @ : list(int32)
            --         ( qsort
            --         , @ : ?
            --             ( $Record
            --             , ?
            --             )
            --         )
            --
            ( Core.let_
                ( ( Label undefined "ys"
                  , Core.app
                      undefined
                      undefined
                      undefined
                  )
                    :| []
                )
                --
                -- in
                --   match(ys : list(int32)) {
                --     | ($Cons : ?, a : int32, b : list(int32)) =>
                --         match() {
                --         }
                --     | $Nil =>
                --         ?
                --   }
                --
                --
                ( Core.match
                    undefined
                    (Core.var (Label (list Core.int32) "ys"))
                    ( Clause
                        undefined
                        undefined
                        <| Clause
                          undefined
                          undefined
                        :| []
                    )
                )
            )
        )
    )
