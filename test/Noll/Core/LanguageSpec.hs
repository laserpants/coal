{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LanguageSpec where

import Noll.Common.List1 (List1, NonEmpty (..))
import Noll.Core.Language.Syntax
import Noll.Label (Label (..))

fixture =
  let_
    -- compose
    undefined
    ( let_
        ( ( Label undefined "_compose_"
          , undefined
          )
            :| [
                 ( undefined
                 , undefined
                 )
               ,
                 ( undefined
                 , undefined
                 )
               ,
                 ( undefined
                 , undefined
                 )
               ,
                 ( undefined
                 , undefined
                 )
               ,
                 ( undefined
                 , undefined
                 )
               ,
                 ( undefined
                 , undefined
                 )
               ,
                 ( undefined
                 , undefined
                 )
               ]
        )
        undefined
    )
