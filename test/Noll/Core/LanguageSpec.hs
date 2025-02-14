module Noll.Core.LanguageSpec where

import Noll.Core.Language.Expr.Syntax 

fixture =
  ELet 
    -- compose
    undefined
    ( ELet
        undefined
        undefined
    )
