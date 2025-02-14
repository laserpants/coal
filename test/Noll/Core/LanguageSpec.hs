module Noll.Core.LanguageSpec where

import Noll.Common.List1 (List1, NonEmpty (..))
import Noll.Core.Language.Expr.Syntax

fixture =
  let_
    -- compose
    undefined
    ( let_
        ( ( undefined
          , undefined
          )
            :| [
                 ( undefined
                 , undefined
                 )
               , ( undefined
                 , undefined
                 )
               , ( undefined
                 , undefined
                 )
               , ( undefined
                 , undefined
                 )
               , ( undefined
                 , undefined
                 )
               , ( undefined
                 , undefined
                 )
               , ( undefined
                 , undefined
                 )
               ]
        )
        undefined
    )
