{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LanguageSpec where

import Noll.Common.List1 (NonEmpty (..))
import Noll.Core.Language.Expr (Clause (..))
import Noll.Core.Language.Syntax (opaque, (~>), list)
import Noll.Label (Label (..))

import qualified Noll.Core.Language.Syntax as Core

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
              (Label (list opaque) "a"
                :| [ Label (list opaque) "b" ]
              )
              (
                Core.match
                  (list opaque)
                  (Core.var (Label (list opaque) "a"))
                  (
                    Clause
                      undefined
                      undefined
                      :| [
                    Clause
                      undefined
                      undefined
                      ]
                  )
              )
          )
            :| [
  --         compare =
  --           fn(a_1, a_2, a_3) =>
  --             match(a1) {
  --               | ?
  --             }
  --           ;
  --
                 ( Label undefined "compare"
                 , undefined
                 )
               ,
                 ( Label undefined "from_int32"
                 , undefined
                 )
               ,
                 ( Label undefined "_forward_application_"
                 , undefined
                 )
               ,
                 ( Label undefined "_not_"
                 , undefined
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
               ,
                 ( Label undefined "gt"
                 , undefined
                 )
               ]
        )
        undefined
    )
