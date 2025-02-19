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

orderedRow :: Core.Type
orderedRow = Core.RExt "compare" (opaque ~> opaque ~> Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 ~> opaque) opaque)

orderedInt32Row :: Core.Type
orderedInt32Row = Core.RExt "compare" (Core.int32 ~> Core.int32 ~> Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 ~> Core.int32) opaque)

maxMinRow :: Core.Type -> Core.Type
maxMinRow r = Core.RExt "max" opaque (Core.RExt "min" opaque r)

-- record({ compare : * -> * -> Ordering | * })
compareDict :: Core.Type
compareDict = Core.record compareRow

-- record({ from_int32 : int32 -> * | * })
fromInt32Dict :: Core.Type
fromInt32Dict = Core.record fromInt32Row

-- record({ compare : * -> * -> Ordering | from_int32 : int32 -> * | * })
orderedDict :: Core.Type
orderedDict = Core.record orderedRow

orderedInt32Dict :: Core.Type
orderedInt32Dict = Core.record orderedInt32Row

ordering :: Core.Type
ordering = Core.TCon "Ordering" []

-- record({ max : 0 | min : 0 | r })
maxMinRecord :: Core.Type -> Core.Type
maxMinRecord r = Core.record (maxMinRow r)

tree :: Core.Type -> Core.Type
tree t = Core.TCon "Tree" [t]

--
-- let
--   _compose_ : (* -> *) -> (* -> *) -> * -> * =
--     fn( f : * -> *
--       , g : * -> *
--       , x : *
--       ) =>
--         @ : * ( f : * -> *
--               , @ : * ( g : * -> *
--                       , x : * ))
--   in
--     let
--       _list_concat_ : list(*) -> list(*) -> list(*) =
--         fn( a : list(*)
--           , b : list(*)
--           ) =>
--             match : list(*) (a : list(*)) {
--               | $Nil : list(*) =>
--                   b : list(*)
--               | ( $Cons : * -> list(*) -> list(*)
--                 , x : *
--                 , xs : list(*)
--                 ) =>
--                   @ : list(*)
--                     ( $Cons : * -> list(*) -> list(*)
--                     , x : *
--                     , @ : list(*)
--                         ( _list_concat : list(*) -> list(*) -> list(*)
--                         , xs : list(*)
--                         , b : list(*)
--                         )
--                     )
--             }
--         ;
--       compare : record({ compare : * -> * -> Ordering | * }) -> * -> * -> Ordering =
--         fn( a_1 : : record({ compare : * -> * -> Ordering | * })
--           , a_2 : *
--           , a_3 : *
--           ) =>
--             match : Ordering (a_1 : record({ compare : * -> * -> Ordering | * })) {
--               | ( $Record : { compare : * -> * -> Ordering | * } -> record({ compare : * -> * -> Ordering | * })
--                 , r_1 : { compare : * -> * -> Ordering | * }
--                 ) =>
--                   select
--                     { compare = f_1 : * -> * -> Ordering | q_1 : * } =
--                       r_1 : { compare : * -> * -> Ordering | * }
--                     in
--                       @ : Ordering
--                         ( f_1 : * -> * -> Ordering
--                         , a_2 : *
--                         , a_3 : *
--                         )
--             }
--         ;
--       from_int32 : record({ from_int32 : int32 -> * | * }) -> int32 -> * =
--         fn( a_1 : record({ from_int32 : int32 -> * | * })
--           , a_2 : int32
--           ) =>
--             match : * (a_1 : record({ from_int32 : int32 -> * | * })) {
--               | ( $Record : { from_int32 : int32 -> * | * } -> record({ from_int32 : int32 -> * | * })
--                 , r_1 : { from_int32 : int32 -> * | * }
--                 ) =>
--                   select
--                     { from_int32 = f_1 : int32 -> * | q_1 : * } =
--                       r_1 : { from_int32 : int32 -> * | * }
--                     in
--                       @ : *
--                         ( f_1 : int32 -> *
--                         , a_2 : int32
--                         )
--             }
--         ;
--       _forward_application_ : * -> (* -> *) -> * =
--         fn(x : *, f : * -> *) =>
--           @ : * (f : * -> *, x : *)
--         ;
--       _not_ : bool -> bool =
--         fn(a : bool) =>
--           if (a : bool) then false else true
--         ;
--       compare__int32 : int32 -> int32 -> Ordering =
--         fn(x : int32, y : int32) =>
--           if (x : int32 [< int32] y : int32)
--             then
--               LessThan : Ordering
--             else
--               if (x : int32 [> int32] y : int32)
--                 then
--                   GreaterThan : Ordering
--                 else
--                   EqualTo : Ordering
--         ;
--       from_int32__int32 : int32 -> int32 =
--         fn(n : int32) =>
--           n : int32
--         ;
--       lte : record({ compare : * -> * -> Ordering | * }) -> * -> * -> bool =
--         fn(d_1 : record({ compare : * -> * -> Ordering | * })) =>
--           fn(x : *) =>
--             fn(y : *) =>
--               match : bool
--                     ( @ : Ordering
--                         ( compare : record({ compare : * -> * -> Ordering | * }) -> * -> * -> Ordering
--                         , d_1 : record({ compare : * -> * -> Ordering | * })
--                         , x : *
--                         , y : *
--                         )) {
--                 | LessThan : Ordering =>
--                     true
--                 | EqualTo : Ordering =>
--                     true
--                 | GreaterThan : Ordering =>
--                     false
--               }
--         ;
--       gt : record({ compare : * -> * -> Ordering | * }) -> * -> * -> bool =
--         fn(d_1 : record({ compare : * -> * -> Ordering | * })) =>
--           fn(x : *) =>
--             @ : * -> bool
--               ( _compose_ : (bool -> bool) -> (* -> bool) -> * -> bool
--               , _not_ : bool -> bool
--               , @ : * -> bool
--                   ( lte : record({ compare : * -> * -> Ordering | * }) -> * -> * -> bool
--                   , d_1 : record({ compare : * -> * -> Ordering | * })
--                   , x : *
--                   ))
--         ;
--       in_range : record({ compare : * -> * -> Ordering | * }) -> record({ max : * | min : * | * }) -> * -> bool =
--         fn(d_1 : record({ compare : * -> * -> Ordering | * })) =>
--           fn( range : record({ max : * | min : * | * })
--             , n : *
--             ) =>
--               match : bool (range : record({ max : * | min : * | * })) {
--                 ( $Record : { max : * | min : * | * } -> record({ max : * | min : * | * })
--                 , row_1 : { max : * | min : * | * }
--                 ) =>
--                   select
--                     { min = min : * | row_2 : { max : * | * } } =
--                       row_1 : { max : * | min : * | * }
--                     in
--                       select
--                         { max = max : * | z : * } =
--                           row_2 : { max : * | * }
--                         in
--                           @ : bool ( gt : record({ compare : * -> * -> Ordering | * }) -> * -> * -> bool
--                                    , d_1 : record({ compare : * -> * -> Ordering | * })
--                                    , n : *
--                                    , min : * )
--                           &&
--                             ( @ : bool ( gt : record({ compare : * -> * -> Ordering | * }) -> * -> * -> bool
--                                        , d_1 : record({ compare : * -> * -> Ordering | * })
--                                        , min : *
--                                        , max : * )
--                               ||
--                               @ : bool ( lte : record({ compare : * -> * -> Ordering | * }) -> * -> * -> bool
--                                        , d_1 : record({ compare : * -> * -> Ordering | * })
--                                        , n : *
--                                        , max : * ))
--               }
--         ;
--       from_list : record({ compare : * -> * -> Ordering | from_int32 : int32 -> * | * }) -> list(*) -> Tree(*)=
--         fn(d_1 : record({ compare : * -> * -> Ordering | from_int32 : int32 -> * | * })) =>
--           fn(list : list(*)) =>
--             let
--               fold_ : list(*) -> record({ max : * | min : * | * }) -> Tree(*) =
--                 fn(a_0 : list(*)) =>
--                   match : record({ max : * | min : * | * }) -> Tree(*) (a_0 : list(*)) {
--                     | ( $Cons : * -> list(*) -> list(*)
--                       , p : *
--                       , g : list(*)
--                       ) =>
--                         fn(range : record({ max : * | min : * | * })) =>
--                           if ( @ : bool
--                                  ( _forward_application_ : * -> (* -> bool) -> bool
--                                  , p : *
--                                  , @ : * -> bool
--                                      ( in_range : record({ compare : * -> * -> Ordering | * }) -> record({ max : * | min : * | * }) -> * -> bool
--                                      , d_1 : record({ compare : * -> * -> Ordering | * })
--                                      , range : record({ max : * | min : * | * })
--                                      )
--                                  )
--                              )
--                             then
--                               @ : Tree(*)
--                                 ( Node : * -> Tree(*) -> Tree(*) -> Tree(*)
--                                 , p : *
--                                 , @ : Tree(*)
--                                     ( fold_ : list(*) -> record({ max : * | min : * | * }) -> Tree(*)
--                                     , g : list(*)
--                                     , @ : record({ max : * | min : * | * })
--                                         ( $Record : { max : * | min : * | * } -> record({ max : * | min : * | * })
--                                         , { min =
--                                               match : * (range : record({ max : * | min : * | * })) {
--                                                 | ($Record : { max : * | min : * | * } -> record({ max : * | min : * | * }), row_2 : { max : * | min : * | * }) =>
--                                                     select
--                                                       { min = m_1 : * | q_2 : { max : * | * } } =
--                                                         row_2 : { max : * | min : * | * }
--                                                       in
--                                                         m_1 : *
--                                               }
--                                           | max = p : *
--                                           | {}
--                                           }
--                                         )
--                                     )
--                                 , @ : Tree(*)
--                                     ( fold_ : list(*) -> record({ max : * | min : * | * }) -> Tree(*)
--                                     , g : list(*)
--                                     , @ : record({ max : * | min : * | * })
--                                         ( $Record : { max : * | min : * | * } -> record({ max : * | min : * | * })
--                                         , { min = p : *
--                                           | max =
--                                               match : * (range : record({ max : * | min : * | * })) {
--                                                 | ($Record : { max : * | min : * | * } -> record({ max : * | min : * | * }), row_2 : { max : * | min : * | * }) =>
--                                                     select
--                                                       { max = m_1 : * | q_2 : { min : * | * } } =
--                                                         row_2 : { max : * | min : * | * }
--                                                       in
--                                                         m_1 : *
--                                               }
--                                           | {}
--                                           }
--                                         )
--                                     )
--                                 )
--                             else
--                               @ : Tree(*)
--                                 ( fold_ : list(*) -> record({ max : * | min : * | * }) -> Tree(*)
--                                 , g : list(*)
--                                 , range : record({ max : * | min : * | * })
--                                 )
--                     | $Nil : list(*) =>
--                         fn(_ : record({ max : * | min : * | * })) =>
--                           Leaf : Tree(*)
--                   }
--               in
--                 @ : Tree(*)
--                   ( fold_ : list(*) -> record({ max : * | min : * | * }) -> Tree(*)
--                   , list : list(*)
--                   , @ : record({ max : * | min : * | {} })
--                       ( $Record : { max : * | min : * | {} } -> record({ max : * | min : * | {} })
--                       , { min = @ : *
--                                   ( from_int32 : record({ from_int32 : int32 -> * | * }) -> int32 -> *
--                                   , d_1 : record({ compare : * -> * -> Ordering | from_int32 : int32 -> * | * })
--                                   , 0 : int32 )
--                         | max = @ : *
--                                   ( from_int32 : record({ from_int32 : int32 -> * | * }) -> int32 -> *
--                                   , d_1 : record({ compare : * -> * -> Ordering | from_int32 : int32 -> * | * })
--                                   , -1 : int32 )
--                         | {}
--                         }
--                       )
--                   )
--         ;
--       flatten : Tree(*) -> list(*) =
--         fn(tree : Tree(*)) =>
--           let
--             fold_ : Tree(*) -> list(*) =
--               fn(a_0 : Tree(*)) =>
--                 match : list(*) (a_0 : Tree(*)) {
--                   | ( Node : * -> Tree(*) -> Tree(*) -> Tree(*)
--                     , y : *
--                     , lhs : Tree(*)
--                     , rhs : Tree(*)
--                     ) =>
--                       @ : list(*)
--                         ( _list_concat_ : list(*) -> list(*) -> list(*)
--                         , @ : list(*)
--                             ( fold_ : Tree(*) -> list(*)
--                             , lhs : Tree(*))
--                         , @ : list(*)
--                             ( $Cons : * -> list(*) -> list(*)
--                             , y : *
--                             , @ : list(*)
--                                 ( fold_ : Tree(*) -> list(*)
--                                 , rhs : Tree(*))))
--                   | Leaf : Tree(*) =>
--                       $Nil : list(*)
--                 }
--             in
--               @ : list(*)
--                 ( fold_ : Tree(*) -> list(*)
--                 , tree : Tree(*)
--                 )
--         ;
--       qsort : record({ compare : * -> * -> Ordering | from_int32 : int32 -> * | * }) -> list(*) -> list(*) =
--         fn(d_1 : record({ compare : * -> * -> Ordering | from_int32 : int32 -> * | * })) =>
--           @ : list(*) -> list(*)
--             ( _compose_ : (Tree(*) -> list(*)) -> (list(*) -> Tree(*)) -> list(*) -> list(*)
--             , flatten : Tree(*) -> list(*)
--             , @ : list(*) -> Tree(*)
--                 ( from_list : record({ compare : * -> * -> Ordering | from_int32 : int32 -> * | * }) -> list(*) -> Tree(*)
--                 , d_1 : record({ compare : * -> * -> Ordering | from_int32 : int32 -> * | * })
--                 ))
--       in
--         let
--           xs =
--             @ : list(int32)
--               ( $Cons : int32 -> list(int32) -> list(int32)
--               , 2 : int32
--               , @ : list(int32)
--                   ( $Cons : int32 -> list(int32) -> list(int32)
--                   , 105 : int32
--                   , @ : list(int32)
--                       ( $Cons : int32 -> list(int32) -> list(int32)
--                       , 103 : int32
--                       , @ : list(int32)
--                         ( $Cons : int32 -> list(int32) -> list(int32)
--                         , 104 : int32
--                         , @ : list(int32)
--                           ( $Cons : int32 -> list(int32) -> list(int32)
--                           , 2 : int32
--                           , @ : list(int32)
--                             ( $Cons : int32 -> list(int32) -> list(int32)
--                             , 106 : int32
--                             , $Nil : list(int32)
--                             ))))))
--           in
--             let
--               ys =
--                 @ : list(int32)
--                   ( qsort : record({ compare : int32 -> int32 -> Ordering | from_int32 : int32 -> int32 | {} }) -> list(int32) -> list(int32)
--                   , @ : record({ compare : int32 -> int32 -> Ordering | from_int32 : int32 -> int32 | {} })
--                       ( $Record : { compare : int32 -> int32 -> Ordering | from_int32 : int32 -> int32 | {} } -> record({ compare : int32 -> int32 -> Ordering | from_int32 : int32 -> int32 | {} })
--                       , { compare = compare__int32 : int32 -> int32 -> Ordering
--                         | from_int32 = from_int32__int32 : int32 -> int32
--                         | {}
--                         }
--                       )
--                   , xs : list(int32)
--                   )
--               in
--                 match : int32 (ys : list(int32)) {
--                   | ($Cons : int32 -> list(int32) -> list(int32), a : int32, b : list(int32)) =>
--                       match(b : list(int32)) {
--                         | ($Cons : int32 -> list(int32) -> list(int32), c : int32, d : list(int32)) =>
--                             match(d : list(int32)) {
--                               | ($Cons : int32 -> list(int32) -> list(int32), e : int32, f : list(int32)) =>
--                                   e : int32
--                             }
--                       }
--                 }
--
fixture :: Expr Core.Type
fixture =
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
                            ( Label (compareRow ~> compareDict) "$Record"
                                <| Label compareRow "r_1"
                                :| []
                            )
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
               ,
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
                            ( Label (fromInt32Row ~> fromInt32Dict) "$Record"
                                <| Label fromInt32Row "r_1"
                                :| []
                            )
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
               ,
                 ( Label (opaque ~> (opaque ~> opaque) ~> opaque) "_forward_application_"
                 , Core.lam
                    (Label opaque "x" <| Label (opaque ~> opaque) "f" :| [])
                    ( Core.app
                        opaque
                        (Core.var (Label (opaque ~> opaque) "f"))
                        (Core.var (Label opaque "x") :| [])
                    )
                 )
               ,
                 ( Label (Core.bool ~> Core.bool) "_not_"
                 , Core.lam
                    (Label Core.bool "a" :| [])
                    ( Core.if_
                        (Core.var (Label Core.bool "a"))
                        (Core.lit (Core.PBool False))
                        (Core.lit (Core.PBool True))
                    )
                 )
               ,
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
               ,
                 ( Label (Core.int32 ~> Core.int32) "from_int32__int32"
                 , Core.lam
                    (Label Core.int32 "n" :| [])
                    (Core.var (Label Core.int32 "n"))
                 )
               ,
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
               ,
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
               ,
                 ( Label (compareDict ~> maxMinRecord opaque ~> opaque ~> Core.bool) "in_range"
                 , Core.lam
                    (Label compareDict "d_1" :| [])
                    ( Core.lam
                        (Label (maxMinRecord opaque) "range" <| Label opaque "n" :| [])
                        ( Core.match
                            Core.bool
                            (Core.var (Label (maxMinRecord opaque) "range"))
                            ( Clause
                                ( Label (maxMinRow opaque ~> maxMinRecord opaque) "$Record"
                                    <| Label (maxMinRow opaque) "row_1"
                                    :| []
                                )
                                ( Core.sel
                                    ( Focus
                                        "min"
                                        (Label opaque "min")
                                        (Label (Core.RExt "max" opaque opaque) "row_2")
                                    )
                                    (Core.var (Label (maxMinRow opaque) "row_1"))
                                    ( Core.sel
                                        ( Focus
                                            "max"
                                            (Label opaque "max")
                                            (Label opaque "z")
                                        )
                                        (Core.var (Label (Core.RExt "max" opaque opaque) "row_2"))
                                        ( Core.op
                                            ( Core.OAnd
                                                ( Core.app
                                                    Core.bool
                                                    (Core.var (Label (compareDict ~> opaque ~> opaque ~> Core.bool) "gt"))
                                                    ( Core.var (Label compareDict "d_1")
                                                        <| Core.var (Label opaque "n")
                                                        <| Core.var (Label opaque "min")
                                                        :| []
                                                    )
                                                )
                                                ( Core.op
                                                    ( Core.OOr
                                                        ( Core.app
                                                            Core.bool
                                                            (Core.var (Label (compareDict ~> opaque ~> opaque ~> Core.bool) "gt"))
                                                            ( Core.var (Label compareDict "d_1")
                                                                <| Core.var (Label opaque "min")
                                                                <| Core.var (Label opaque "max")
                                                                :| []
                                                            )
                                                        )
                                                        ( Core.app
                                                            Core.bool
                                                            (Core.var (Label (compareDict ~> opaque ~> opaque ~> Core.bool) "lte"))
                                                            ( Core.var (Label compareDict "d_1")
                                                                <| Core.var (Label opaque "n")
                                                                <| Core.var (Label opaque "max")
                                                                :| []
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                                :| []
                            )
                        )
                    )
                 )
               ,
                 ( Label (orderedDict ~> list opaque ~> tree opaque) "from_list"
                 , Core.lam
                    (Label orderedDict "d_1" :| [])
                    ( Core.lam
                        (Label (list opaque) "list" :| [])
                        ( Core.let_
                            ( ( Label (list opaque ~> maxMinRecord opaque ~> tree opaque) "fold_"
                              , Core.lam
                                  (Label (list opaque) "a_0" :| [])
                                  ( Core.match
                                      (maxMinRecord opaque ~> tree opaque)
                                      (Core.var (Label (list opaque) "a_0"))
                                      ( Clause
                                          ( Label (opaque ~> list opaque ~> list opaque) "$Cons"
                                              <| Label opaque "p"
                                              <| Label (list opaque) "g"
                                              :| []
                                          )
                                          ( Core.lam
                                              (Label (maxMinRecord opaque) "range" :| [])
                                              ( Core.if_
                                                  ( Core.app
                                                      Core.bool
                                                      (Core.var (Label (opaque ~> (opaque ~> opaque) ~> opaque) "_forward_application_"))
                                                      ( Core.var (Label opaque "p")
                                                          <| Core.app
                                                            (opaque ~> Core.bool)
                                                            (Core.var (Label (compareDict ~> maxMinRecord opaque ~> opaque ~> Core.bool) "in_range"))
                                                            ( Core.var (Label compareDict "d_1")
                                                                <| Core.var (Label (maxMinRecord opaque) "range")
                                                                :| []
                                                            )
                                                          :| []
                                                      )
                                                  )
                                                  -- then
                                                  ( Core.app
                                                      (tree opaque)
                                                      (Core.var (Label (opaque ~> tree opaque ~> tree opaque ~> tree opaque) "Node"))
                                                      ( Core.var (Label opaque "p")
                                                          <| Core.app
                                                            (tree opaque)
                                                            (Core.var (Label (list opaque ~> maxMinRecord opaque ~> tree opaque) "fold_"))
                                                            ( Core.var (Label (list opaque) "g")
                                                                <| Core.app
                                                                  (maxMinRecord opaque)
                                                                  (Core.var (Label (maxMinRow opaque ~> maxMinRecord opaque) "$Record"))
                                                                  ( Core.ext
                                                                      (Label opaque "min")
                                                                      ( Core.match
                                                                          opaque
                                                                          (Core.var (Label (maxMinRecord opaque) "range"))
                                                                          ( Clause
                                                                              ( Label (maxMinRow opaque ~> maxMinRecord opaque) "$Record"
                                                                                  <| Label (maxMinRow opaque) "row_2"
                                                                                  :| []
                                                                              )
                                                                              ( Core.sel
                                                                                  ( Focus
                                                                                      "min"
                                                                                      (Label opaque "m_1")
                                                                                      (Label (Core.RExt "max" opaque opaque) "q_2")
                                                                                  )
                                                                                  (Core.var (Label (maxMinRow opaque) "row_2"))
                                                                                  (Core.var (Label opaque "m_1"))
                                                                              )
                                                                              :| []
                                                                          )
                                                                      )
                                                                      ( Core.ext
                                                                          (Label opaque "max")
                                                                          (Core.var (Label opaque "p"))
                                                                          Core.nil
                                                                      )
                                                                      :| []
                                                                  )
                                                                :| []
                                                            )
                                                          <| Core.app
                                                            (tree opaque)
                                                            (Core.var (Label (list opaque ~> maxMinRecord opaque ~> tree opaque) "fold_"))
                                                            ( Core.var (Label (list opaque) "g")
                                                                <| Core.app
                                                                  (maxMinRecord opaque)
                                                                  (Core.var (Label (maxMinRow opaque ~> maxMinRecord opaque) "$Record"))
                                                                  ( Core.ext
                                                                      (Label opaque "min")
                                                                      (Core.var (Label opaque "p"))
                                                                      ( Core.ext
                                                                          (Label opaque "max")
                                                                          ( Core.match
                                                                              opaque
                                                                              (Core.var (Label (maxMinRecord opaque) "range"))
                                                                              ( Clause
                                                                                  ( Label (maxMinRow opaque ~> maxMinRecord opaque) "$Record"
                                                                                      <| Label (maxMinRow opaque) "row_2"
                                                                                      :| []
                                                                                  )
                                                                                  ( Core.sel
                                                                                      ( Focus
                                                                                          "max"
                                                                                          (Label opaque "m_1")
                                                                                          (Label (Core.RExt "min" opaque opaque) "q_2")
                                                                                      )
                                                                                      (Core.var (Label (maxMinRow opaque) "row_2"))
                                                                                      (Core.var (Label opaque "m_1"))
                                                                                  )
                                                                                  :| []
                                                                              )
                                                                          )
                                                                          Core.nil
                                                                      )
                                                                      :| []
                                                                  )
                                                                :| []
                                                            )
                                                          :| []
                                                      )
                                                  )
                                                  -- else
                                                  ( Core.app
                                                      (tree opaque)
                                                      (Core.var (Label (list opaque ~> maxMinRecord opaque ~> tree opaque) "fold_"))
                                                      ( Core.var (Label (list opaque) "g")
                                                          <| Core.var (Label (maxMinRecord opaque) "range")
                                                          :| []
                                                      )
                                                  )
                                              )
                                          )
                                          <| Clause
                                            (Label (list opaque) "$Nil" :| [])
                                            ( Core.lam
                                                (Label (maxMinRecord opaque) "_" :| [])
                                                (Core.var (Label (tree opaque) "Leaf"))
                                            )
                                          :| []
                                      )
                                  )
                              )
                                :| []
                            )
                            ( Core.app
                                (tree opaque)
                                (Core.var (Label (list opaque ~> maxMinRecord opaque ~> tree opaque) "fold_"))
                                ( Core.var (Label (list opaque) "list")
                                    <| Core.app
                                      (maxMinRecord opaque)
                                      (Core.var (Label (maxMinRow opaque ~> maxMinRecord opaque) "$Record"))
                                      ( Core.ext
                                          (Label opaque "min")
                                          ( Core.app
                                              opaque
                                              (Core.var (Label (orderedDict ~> Core.int32 ~> opaque) "from_int32"))
                                              ( Core.var (Label orderedDict "d_1")
                                                  <| Core.lit (Core.PInt32 0)
                                                  :| []
                                              )
                                          )
                                          ( Core.ext
                                              (Label opaque "max")
                                              ( Core.app
                                                  opaque
                                                  (Core.var (Label (orderedDict ~> Core.int32 ~> opaque) "from_int32"))
                                                  ( Core.var (Label orderedDict "d_1")
                                                      <| Core.lit (Core.PInt32 (-1))
                                                      :| []
                                                  )
                                              )
                                              Core.nil
                                          )
                                          :| []
                                      )
                                    :| []
                                )
                            )
                        )
                    )
                 )
               ,
                 ( Label (tree opaque ~> list opaque) "flatten"
                 , Core.lam
                    (Label (tree opaque) "tree" :| [])
                    ( Core.let_
                        ( ( Label (tree opaque ~> list opaque) "fold_"
                          , Core.lam
                              (Label (tree opaque) "a_0" :| [])
                              ( Core.match
                                  (list opaque)
                                  (Core.var (Label (tree opaque) "a_0"))
                                  ( Clause
                                      ( Label (opaque ~> tree opaque ~> tree opaque ~> tree opaque) "Node"
                                          <| Label opaque "y"
                                          <| Label (tree opaque) "lhs"
                                          <| Label (tree opaque) "rhs"
                                          :| []
                                      )
                                      ( Core.app
                                          (list opaque)
                                          (Core.var (Label (list opaque ~> list opaque ~> list opaque) "_list_concat_"))
                                          ( Core.app
                                              (list opaque)
                                              (Core.var (Label (tree opaque ~> list opaque) "fold_"))
                                              (Core.var (Label (tree opaque) "lhs") :| [])
                                              <| Core.app
                                                (list opaque)
                                                (Core.var (Label (opaque ~> list opaque ~> list opaque) "$Cons"))
                                                ( Core.var (Label opaque "y")
                                                    <| Core.app
                                                      (list opaque)
                                                      (Core.var (Label (tree opaque ~> list opaque) "fold_"))
                                                      (Core.var (Label (tree opaque) "rhs") :| [])
                                                    :| []
                                                )
                                              :| []
                                          )
                                      )
                                      <| Clause
                                        (Label (tree opaque) "Leaf" :| [])
                                        (Core.var (Label (list opaque) "$Nil"))
                                      :| []
                                  )
                              )
                          )
                            :| []
                        )
                        ( Core.app
                            (list opaque)
                            (Core.var (Label (tree opaque ~> list opaque) "fold_"))
                            (Core.var (Label (tree opaque) "tree") :| [])
                        )
                    )
                 )
               ,
                 ( Label (orderedDict ~> list opaque ~> list opaque) "qsort"
                 , Core.lam
                    (Label orderedDict "d_1" :| [])
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
                              (Core.var (Label (orderedDict ~> list opaque ~> tree opaque) "from_list"))
                              (Core.var (Label orderedDict "d_1") :| [])
                            :| []
                        )
                    )
                 )
               ]
        )
        ( Core.let_
            ( ( Label (list Core.int32) "xs"
              , Core.app
                  (list Core.int32)
                  (Core.var (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons"))
                  ( Core.lit (Core.PInt32 2)
                      <| Core.app
                        (list Core.int32)
                        (Core.var (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons"))
                        ( Core.lit (Core.PInt32 105)
                            <| Core.app
                              (list Core.int32)
                              (Core.var (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons"))
                              ( Core.lit (Core.PInt32 103)
                                  <| Core.app
                                    (list Core.int32)
                                    (Core.var (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons"))
                                    ( Core.lit (Core.PInt32 104)
                                        <| Core.app
                                          (list Core.int32)
                                          (Core.var (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons"))
                                          ( Core.lit (Core.PInt32 2)
                                              <| Core.app
                                                (list Core.int32)
                                                (Core.var (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons"))
                                                ( Core.lit (Core.PInt32 106)
                                                    :| [Core.var (Label (list Core.int32) "$Nil")]
                                                )
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
            ( Core.let_
                ( ( Label (list Core.int32) "ys"
                  , Core.app
                      (list Core.int32)
                      (Core.var (Label (orderedInt32Dict ~> list Core.int32 ~> list Core.int32) "qsort"))
                      ( Core.app
                          orderedInt32Dict
                          (Core.var (Label (orderedInt32Row ~> orderedInt32Dict) "$Record"))
                          ( Core.ext
                              (Label (Core.int32 ~> Core.int32 ~> ordering) "compare")
                              (Core.var (Label (Core.int32 ~> Core.int32 ~> ordering) "compare__int32"))
                              ( Core.ext
                                  (Label (Core.int32 ~> Core.int32) "from_int32")
                                  (Core.var (Label (Core.int32 ~> Core.int32) "from_int32__int32"))
                                  Core.nil
                              )
                              :| []
                          )
                          <| Core.var (Label (list Core.int32) "xs")
                          :| []
                      )
                  )
                    :| []
                )
                ( Core.match
                    Core.int32
                    (Core.var (Label (list Core.int32) "ys"))
                    ( Clause
                        (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons" <| Label Core.int32 "a" <| Label (list Core.int32) "b" :| [])
                        ( Core.match
                            Core.int32
                            (Core.var (Label (list Core.int32) "b"))
                            ( Clause
                                (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons" <| Label Core.int32 "c" <| Label (list Core.int32) "d" :| [])
                                ( Core.match
                                    Core.int32
                                    (Core.var (Label (list Core.int32) "d"))
                                    ( Clause
                                        (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons" <| Label Core.int32 "e" <| Label (list Core.int32) "f" :| [])
                                        (Core.var (Label Core.int32 "e"))
                                        :| []
                                    )
                                )
                                :| []
                            )
                        )
                        :| []
                    )
                )
            )
        )
    )
