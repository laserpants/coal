{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LanguageSpec where

import Noll.Common.List1 (NonEmpty (..), (<|))
import Noll.Core.Language.Expr (Clause (..), Expr)
import Noll.Core.Language.Syntax (list, opaque, (~>))
import Noll.Label (Label (..))

import qualified Noll.Core.Language.Prim as Core
import qualified Noll.Core.Language.Syntax as Core

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
    --             match(a : list(0)) : list(0) {
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
                 --         compare : $Record(compare : ? | ?) -> ? =
                 --           fn(a_1, a_2, a_3) =>
                 --             match(a1 : ?) : ? {
                 --               | $Record(r_1) : ? =>
                 --             }
                 --           ;
                 --

                 ( Label undefined "compare"
                 , Core.lam
                    ( Label undefined "a_1"
                        <| Label undefined "a_2"
                        <| Label undefined "a_3"
                        :| []
                    )
                    ( Core.match
                        undefined
                        (Core.var (Label undefined "a_1"))
                        ( Clause
                            undefined
                            undefined
                            :| []
                        )
                    )
                 )
               ,
                 ( Label undefined "from_int32"
                 , undefined
                 )
               , --         _forward_application_ =
                 --           fn(x, f) =>
                 --             @f(x)
                 --           ;
                 --

                 ( Label undefined "_forward_application_"
                 , undefined
                 )
               , --         _not_ =
                 --           fn(a) =>
                 --             if a then false else true
                 --           ;
                 --

                 ( Label (Core.bool ~> Core.bool) "_not_"
                 , Core.lam
                    (Label Core.bool "a" :| [])
                    ( Core.if_
                        Core.bool
                        (Core.var (Label Core.bool "a"))
                        (Core.lit (Core.PBool False))
                        (Core.lit (Core.PBool True))
                    )
                 )
               ,
                 ( Label undefined "compare__int32"
                 , undefined
                 )
               ,
                 ( Label undefined "from_int32__int32"
                 , undefined
                 )
               ,
                 ( Label undefined "lte"
                 , undefined
                 )
               , --         gt =
                 --           fn(d_1) =>
                 --             fn(x) =>
                 --               @_compose_(_not_, @lte(d_1, x))
                 --           ;
                 --

                 ( Label undefined "gt"
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
               ]
        )
        undefined
    )
